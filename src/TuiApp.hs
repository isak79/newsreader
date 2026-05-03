{-# LANGUAGE OverloadedStrings #-}

module TuiApp(runApp) where

import ParseFeed (Entry(..), fallbackEntry)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import qualified Graphics.Vty as V
import qualified Data.Text as T
import System.Process (callProcess)
import Control.Monad.IO.Class (liftIO)
import System.Info
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import qualified Data.Maybe as M
import Brick.Widgets.Center (hCenter, hCenterLayer)
import qualified Data.Ord as D
import Db
import qualified Data.List as L
import Lens.Micro
import Data.Maybe (fromJust)
import ParseNews


blueAttr, greenAttr, yellowAttr, timeAttr, readAttr, warningAttr, defaultAttr :: AttrName
readAttr    = attrName "readBorder"
blueAttr    = attrName "title"
greenAttr   = attrName "selectedTitle"
yellowAttr  = attrName "yellow"
timeAttr    = attrName "time"
warningAttr = attrName "warningAttr"
defaultAttr = attrName "default"

-- | The main function that ties the whole program together
runApp :: IO ()
runApp = do
  initializeTables
  tuiState <- buildState
  let app = App { appAttrMap      = const $ attrMap V.defAttr [ (blueAttr, fg V.blue)
                                                              , (greenAttr, fg V.green)
                                                              , (yellowAttr, fg V.yellow)
                                                              , (warningAttr, fg V.red)
                                                              , (defaultAttr, fg V.white) ]
                , appStartEvent   = return ()
                , appHandleEvent  = handleTuiEvent
                , appChooseCursor = neverShowCursor
                , appDraw         = drawTui }
  _ <- defaultMain app tuiState
  pure ()

-- | The app event handler
handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent ev = do
  ts' <- get
  if warning ts' /= Nothing
    then modify (\s -> s { warning = Nothing })
    else pure ()
  ts <- get
  case (currentDisplay ts,buttonPressed ts) of
    (ShowFeeds, Button 'n')       -> addFeed ev
    (ShowMailboxList, Button 'n') -> addMailbox ev
    (ShowFeeds, Button 'e')       -> renameFeed ev
    (ShowMailboxList, Button 'e') -> renameMailbox ev
    (_, Button 'D')               -> handleDelete ev
    _                             -> handleNormal ev


handleDelete ev = do
  ts <- get
  case (currentDisplay ts, ev) of
    (ShowFeeds, VtyEvent (V.EvKey (V.KChar 'Y') [])) -> do
      deleteFeed $ fst $ getCurrent $ feedList ts
      modify $ setButtonPressed None
      feeds <- liftIO safeFeeds
      mb    <- liftIO fillMailboxes
      modify $ setFeedList $ M.fromJust $ fromList feeds
      modify $ setMailBoxes mb
    (ShowMailboxList, VtyEvent (V.EvKey (V.KChar 'Y') [])) -> do
      let curMbName = fst $ getCurrent $ mailBoxes ts
      modify $ setButtonPressed None
      if Prelude.any (\feed -> (snd feed) == curMbName) (feedList ts)
        then modify  (\s -> s { warning = Just "Can not delete mailbox that subscribes to feeds" })
        else do
          deleteMailbox curMbName
          refillMailboxes
      pure ()
    _                                              -> modify $ setButtonPressed None

renameMailbox :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
renameMailbox ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> do
    modify $ setButtonPressed None
    modify (\s -> s { addMailboxEditor = editorText AddMailboxEditor (Just 1) $ T.pack "" })
  (VtyEvent (V.EvKey V.KEnter [])) -> do
    ts <- get
    let mbName      = T.strip . T.unlines . getEditContents $ addMailboxEditor ts
        mbs         = mailBoxes ts
        oldName      = fst $ getCurrent mbs
        newMbs = updateCurrentItem (mbName, snd $ getCurrent mbs) mbs
    modify $ setMailBoxes newMbs
    modify $ setButtonPressed None
    modify (\s -> s { addMailboxEditor = editorText AddMailboxEditor (Just 1) $ T.pack "" })
    modify $ setMailBoxes newMbs
    modify $ setFeedList ((\(u,t) -> if t == oldName then (u,mbName) else (u,t)) <$> feedList ts)
    liftIO $ updateMailboxName (oldName) mbName
  _      -> zoom editorStateL (handleEditorEvent ev)
  where
    editorStateL = lens addMailboxEditor (\s e -> s { addMailboxEditor = e })


entryL :: Lens' TuiState Entry
entryL = lens getter setter
  where
    getter ts =
      let (_,box) = getCurrent $ mailBoxes ts
      in getCurrent box
    setter ts e =
      let (name,box) = getCurrent $ mailBoxes ts
          box'       = updateCurrentItem e box
      in (ts {mailBoxes = updateCurrentItem (name,box') $ mailBoxes ts})

renameFeed :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
renameFeed ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> do
    modify $ setButtonPressed None
    modify (\s -> s { addFeedEditor = editorText AddFeedEditor (Just 1) $ T.pack "" })
  (VtyEvent (V.EvKey V.KEnter [])) -> do
    ts <- get
    let url         = T.strip . T.unlines . getEditContents $ addFeedEditor ts
        fl          = feedList ts
        oldUrl      = fst $ getCurrent fl
        newFeedList = updateCurrentItem (url, snd $ getCurrent fl) fl
    modify $ setNewFeedUrl (Just url)
    modify $ setButtonPressed None
    modify (\s -> s { addFeedEditor = editorText AddFeedEditor (Just 1) $ T.pack "" })
    modify $ setFeedList newFeedList
    liftIO $ updateFeedUrl oldUrl url
  _      -> zoom editorStateL (handleEditorEvent ev)
  where
    editorStateL = lens addFeedEditor (\s e -> s { addFeedEditor = e })


addMailbox :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
addMailbox ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> modify $ setButtonPressed None
  (VtyEvent (V.EvKey V.KEnter [])) -> do
    ts <- get
    let name = T.strip . T.unlines . getEditContents $ addMailboxEditor ts
    mb <- gets mailBoxes
    if getCurrent (mailBoxes ts) == ("No mailbox", Zipper [] fallbackEntry [])
      then
        modify $ setMailBoxes (Zipper [] (name,Zipper [] fallbackEntry []) [])
      else
        modify $ setMailBoxes (add (name,Zipper [] fallbackEntry []) mb)
    insertMailbox name
    modify $ setButtonPressed None
    modify (\s -> s { addMailboxEditor = editorText AddMailboxEditor (Just 1) $ T.pack "" })
  _                              -> zoom editorStateL (handleEditorEvent ev)
  where
    editorStateL = lens addMailboxEditor (\s e -> s { addMailboxEditor = e })

-- | Handles the keypresses in editor mode, i.e. adding new mailboxes and feeds
addFeed :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
addFeed ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> do
    modify $ setButtonPressed None
    modify (\s -> s { addFeedEditor = editorText AddFeedEditor (Just 1) $ T.pack "" })
  (VtyEvent (V.EvKey V.KEnter [])) -> do
    ts <- get
    let url = T.strip . T.unlines . getEditContents $ addFeedEditor ts
    modify $ setNewFeedUrl (Just url)
    modify $ setCurrentDisplay ChooseMailbox
    modify (\s -> s { addFeedEditor = editorText AddFeedEditor (Just 1) $ T.pack "" })
  _                              -> zoom editorStateL (handleEditorEvent ev)
  where
    editorStateL = lens addFeedEditor (\s e -> s { addFeedEditor = e })

-- | Handles keypresses when not in editor mode
handleNormal :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleNormal (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleNormal (VtyEvent (V.EvKey (V.KChar 'j') [])) = switchItem Next
handleNormal (VtyEvent (V.EvKey (V.KChar 'k') [])) = switchItem Prev
handleNormal (VtyEvent (V.EvKey V.KDown []))       = switchItem Next
handleNormal (VtyEvent (V.EvKey V.KUp []))         = switchItem Prev
handleNormal (VtyEvent (V.EvKey (V.KChar 'd') [])) = changeShowDesc
handleNormal (VtyEvent (V.EvKey (V.KChar 'g') [])) = switchItem Top
handleNormal (VtyEvent (V.EvKey (V.KChar 'G') [])) = switchItem Bottom
handleNormal (VtyEvent (V.EvKey (V.KChar '?') [])) = toggleShowHelp
handleNormal (VtyEvent (V.EvKey (V.KChar '-') [])) = modify $ setCurrentDisplay ShowMailboxList
handleNormal (VtyEvent (V.EvKey V.KEnter []))      = activateItem
handleNormal (VtyEvent (V.EvKey V.KEsc []))        = abort
handleNormal (VtyEvent (V.EvKey (V.KChar 'r') [])) = pressR
handleNormal (VtyEvent (V.EvKey (V.KChar 'u') [])) = pressU
handleNormal (VtyEvent (V.EvKey (V.KChar 'm') [])) = do
  ts <- get
  case currentDisplay ts of
    ShowEntries -> modify $ setButtonPressed (Button 'm')
    ShowFeeds   -> do
      feed <- gets feedList
      let currURL = fst $ getCurrent feed
      modify $ setNewFeedUrl (Just currURL)
      modify $ setCurrentDisplay ChooseMailbox
      modify $ setButtonPressed (Button 'm')
    _           -> pure ()
handleNormal (VtyEvent (V.EvKey (V.KChar 'n') [])) = modify $ setButtonPressed (Button 'n')
handleNormal (VtyEvent (V.EvKey (V.KChar 'f') [])) = modify $ setCurrentDisplay ShowFeeds
handleNormal (VtyEvent (V.EvKey (V.KChar 'e') [])) = do
  ts <- get
  case currentDisplay ts of
    ShowFeeds -> do
      let url = fst $ getCurrent $ feedList ts
      if url == "No url"
        then
          modify (\s -> s {warning = Just "Add a feed first!"})
        else do
          modify (\s -> s { addFeedEditor = editorText AddFeedEditor (Just 1) url })
          modify $ setButtonPressed $ Button 'e'
    ShowMailboxList -> do
      if getCurrent (mailBoxes ts) == ("No mailbox", Zipper [] fallbackEntry [])
        then
          modify (\s -> s {warning = Just "Add a mailbox first!"})
        else do
          let mbName = fst $ getCurrent $ mailBoxes ts
          modify (\s -> s { addMailboxEditor = editorText AddMailboxEditor (Just 1) mbName })
          modify $ setButtonPressed $ Button 'e'
          pure ()
    _         -> pure ()
handleNormal (VtyEvent (V.EvKey (V.KChar 'D') [])) = do
  ts <- get
  case currentDisplay ts of
    ShowFeeds       -> modify $ setButtonPressed (Button 'D')
    ShowMailboxList -> modify $ setButtonPressed (Button 'D')
    _         -> pure ()
handleNormal _ = pure ()

abort :: EventM ResourceName TuiState ()
abort = do
  cd <- gets currentDisplay
  case cd of
    ChooseMailbox -> do
      modify $ setButtonPressed None
      modify $ setCurrentDisplay ShowFeeds
    _             -> modify $ setButtonPressed None
  pure ()

-- | Fetches every feeed user is currently subscribed to, updates the database, and loads everything into memory
refillMailboxes :: EventM ResourceName TuiState ()
refillMailboxes = do
  liftIO refreshAll
  mb <- liftIO fillMailboxes
  modify $ setMailBoxes mb
  modify $ setCurrentDisplay ShowMailboxList

-- | Helper function for what happens when user presses 'U', mainly for marking current entry as unread
pressU :: EventM ResourceName TuiState ()
pressU = do
  bp <- gets buttonPressed
  case bp of
    Button 'm' -> do
      markEntry False
      modify $ setButtonPressed None
    _ -> return ()


-- | Helper function for what happens when user presses 'R', used for marking current entry as read, and for refreshing mailboxes
pressR :: EventM ResourceName TuiState ()
pressR = do
  bp <- gets buttonPressed
  case bp of
    Button 'm' -> do
      markEntry True
      modify $ setButtonPressed None
    _       -> refillMailboxes

-- | Update the buttonPressed TuiState element
setButtonPressed :: ButtonPressed Char -> TuiState -> TuiState
setButtonPressed c t = t { buttonPressed = c }

data Dir = Next | Prev | Top | Bottom
  deriving Eq

-- | Moves the current relevant zipper the specified direction
switchItem :: Dir -> EventM ResourceName TuiState ()
switchItem switchTo = do
  mb <- gets mailBoxes
  cd <- gets currentDisplay
  case cd of
    ShowEntries
              -> do
                let (curMailBoxName, curMailBox)   = getCurrent mb
                let newMailBox   = func curMailBox
                let newMailBoxes = updateCurrentItem (curMailBoxName,newMailBox) mb
                modify $ setMailBoxes newMailBoxes
    ShowMailboxList -> zoom mailBoxesL $ modify func
    ChooseMailbox   -> zoom mailBoxesL $ modify func
    ShowFeeds       -> zoom feedsL $ modify func
    _               -> pure ()
    where
      func = case switchTo of
        Next   -> nextItem
        Prev   -> prevItem
        Top    -> firstItem
        Bottom -> lastItem
      feedsL :: Lens' TuiState (Zipper (URL, T.Text))
      feedsL = lens feedList (\s e -> s { feedList = e })
      mailBoxesL :: Lens' TuiState MailBoxes
      mailBoxesL = lens mailBoxes (\s e -> s { mailBoxes = e })

-- | Either enter a mailbox, or if in one, open the entries link
setNewFeedUrl :: Maybe URL -> TuiState -> TuiState
setNewFeedUrl url ts = ts { newFeedUrl = url }

activateItem :: EventM ResourceName TuiState ()
activateItem = do
  curDisplay    <- gets currentDisplay
  bp            <- gets buttonPressed
  case curDisplay of
     ShowMailboxList  -> modify $ setCurrentDisplay ShowEntries
     ChooseMailbox    -> do
        mb <- gets mailBoxes
        let currMb = getCurrent mb
        url <- gets newFeedUrl
        case bp of
          Button 'n' -> addFeedToMailbox (M.fromJust url) $ fst currMb
          Button 'm' -> moveFeed (M.fromJust url) $ fst currMb
          _          -> pure ()
        modify $ setCurrentDisplay ShowFeeds
        modify $ setNewFeedUrl Nothing
        modify $ setButtonPressed None
        feed <- liftIO safeFeeds
        modify $ setFeedList $ M.fromJust $ fromList feed
     ShowFeeds -> pure ()
     ShowEntries    -> do
        mailBoxes   <- gets mailBoxes
        let currEntry = getCurrent $ snd $ getCurrent mailBoxes
        if currEntry /= fallbackEntry then do
          markEntry True
          case article currEntry of
            Just _  -> modify $ setCurrentDisplay ShowArticle
            Nothing -> liftIO $ openUrl $ T.unpack $ source currEntry
        else pure ()
     ShowArticle -> pure ()

-- | Update the states display element
setCurrentDisplay :: CurrentDisplay -> TuiState -> TuiState
setCurrentDisplay cd ts = ts { currentDisplay = cd }

-- | Open the current enties URL

-- | Update the read element of an entry to be either True or False
markEntry b = do
  mailBoxes   <- gets mailBoxes
  let currMailboxTuple = getCurrent mailBoxes
  let currMailbox = snd $ currMailboxTuple
  let currEntry = getCurrent currMailbox
  let readCurrentry = setRead b currEntry
  let updatedCurrMailbox = updateCurrentItem readCurrentry currMailbox
  let updatedMailboxtuple = (fst currMailboxTuple, updatedCurrMailbox)
  let newMailboxes = updateCurrentItem updatedMailboxtuple mailBoxes
  liftIO $ readEntry b currEntry
  modify $ setMailBoxes newMailboxes



setRead :: Bool -> Entry -> Entry
setRead b e = e { isRead = b }

openUrl :: String -> IO ()
openUrl url = case os of
  "darwin"  -> callProcess "open" [url]
  "linux"   -> callProcess "xdg-open" [url]
  "mingw32" -> callProcess "cmd" ["/c", "start", "", url]
  _         -> putStrLn $ "open manually: " ++ url

data ResourceName = EntriesViewport | FeedsViewport | MailboxesViewport | AddMailboxEditor | AddFeedEditor | RenameEditor
  deriving (Show, Eq, Ord)

toggleShowHelp :: EventM ResourceName TuiState ()
toggleShowHelp = do
  sh <- gets showHelp
  modify $ setShowHelp $ not sh

setShowHelp :: Bool -> TuiState -> TuiState
setShowHelp b t = t { showHelp = b}

setFeedList :: Zipper (URL, T.Text) -> TuiState -> TuiState
setFeedList f ts = ts { feedList = f }

safeFeeds :: IO [(T.Text, T.Text)]
safeFeeds = do
  feeds <- getFeeds
  let safe = if null feeds then [("No url", "No feed")] else feeds
  pure safe

-- | Build the initial TUI state
buildState :: IO TuiState
buildState = do
  mailboxes <- fillMailboxes
  let empty = mailboxes == Zipper [] ("No mailbox", Zipper [] fallbackEntry []) []
  feeds     <- safeFeeds
  pure TuiState { currentDisplay   = ShowMailboxList
                , warning          = Nothing
                , showDesc         = False
                , showHelp         = False
                , mailBoxes        = mailboxes
                , buttonPressed    = None
                , addFeedEditor    = editorText AddFeedEditor (Just 1) $ T.pack ""
                , addMailboxEditor = editorText AddMailboxEditor (Just 1) $ T.pack ""
                , feedList         = M.fromJust $ fromList feeds
                , newFeedUrl       = Nothing 
                , emptyMailboxList   = empty }

-- | Fetch every feed, update database and memory
fillMailboxes :: IO MailBoxes
fillMailboxes = do
  mbs <- fetchMailboxes
  mailboxes0 <- if null mbs
      then do
        pure [("No mailbox", Zipper [] fallbackEntry [])]
      else traverse mkMailbox mbs
  let mailboxes = M.fromJust $ fromList mailboxes0
  pure mailboxes
      where
        mkMailbox mbName = do
          e <- fetchEntries mbName
          let mb = maybe (Zipper [] fallbackEntry []) id (fromList $ entrySort e)
          pure (mbName, mb)
        entrySort = L.sortOn (D.Down . M.maybe epoch id . pubTime)
        epoch = UTCTime (fromGregorian 1 1 1970) (secondsToDiffTime 1)


-- setMailBox :: MailBoxes -> TuiState -> TuiState
-- setMailBox mb ts = ts { mailBoxes = mb }


setShowDesc :: Bool -> TuiState -> TuiState
setShowDesc b t = t { showDesc = b }


changeShowDesc :: EventM ResourceName TuiState ()
changeShowDesc = do
  prev <- gets showDesc
  modify $ setShowDesc $ not prev


-- | The datastructure that handles the displayed entries/mailboxes, and the logic of scrolling through them
data Zipper a = Zipper [a] a [a]
  deriving (Show, Functor, Eq, Foldable)

addBeforeCurrent :: a -> Zipper a -> Zipper a
addBeforeCurrent a (Zipper as b cs) = Zipper (a:as) b cs

updateElem :: (a -> Bool) -> a -> Zipper a -> Zipper a
updateElem f element = fmap (\a -> if f a then element else a)

countMatching :: (a -> Bool) -> Zipper a -> Int
countMatching func (Zipper as b cs) = length $ filter func $ as <> [b] <> cs

-- copied from Data.List source code
unsnoc :: [a] -> Maybe ([a], a)
unsnoc   = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

firstItem :: Zipper a -> Zipper a
firstItem z@(Zipper [] _ _) = z
firstItem (Zipper xs y zs)  = Zipper [] r (reverse h ++ y:zs)
  where
    (h,r) = M.fromJust $ unsnoc xs


lastItem :: Zipper a -> Zipper a
lastItem z@(Zipper _ _ []) = z
lastItem (Zipper xs y zs)  = Zipper (reverse h ++ y:xs) r []
  where
    (h,r) = M.fromJust $ unsnoc zs


nextItem :: Zipper a -> Zipper a
nextItem z@(Zipper _ _ [])    = z
nextItem (Zipper xs y (z:zs)) = Zipper (y:xs) z zs

prevItem :: Zipper a -> Zipper a
prevItem z@(Zipper [] _ _)    = z
prevItem (Zipper (x:xs) y zs) = Zipper xs x (y:zs)

fromList :: [a] -> Maybe (Zipper a)
fromList []     = Nothing
fromList (x:xs) = Just (Zipper [] x xs)

toList :: Zipper a -> [a]
toList (Zipper xs y zs) = reverse xs ++ y : zs

getCurrent :: Zipper a -> a
getCurrent (Zipper _ y _) = y

updateCurrentItem :: a -> Zipper a -> Zipper a
updateCurrentItem z (Zipper xs _ zs) = Zipper xs z zs

add :: a -> Zipper a -> Zipper a
add a (Zipper as b cs) = Zipper as b (cs <> [a])


type MailboxName = T.Text
type Article     = T.Text
type Mailbox     = Zipper Entry
type MailBoxes   = Zipper (MailboxName, Mailbox)

data CurrentDisplay = ShowMailboxList | ShowEntries | ShowFeeds | ChooseMailbox | ShowArticle
  deriving Eq

data ButtonPressed x = Button x | None
  deriving Eq

data TuiState = TuiState { currentDisplay   :: CurrentDisplay
                         , showDesc         :: Bool
                         , showHelp         :: Bool
                         , mailBoxes        :: MailBoxes
                         , warning          :: Maybe T.Text
                         , buttonPressed    :: ButtonPressed Char
                         , addFeedEditor    :: Editor T.Text ResourceName
                         , addMailboxEditor :: Editor T.Text ResourceName
                         , newFeedUrl       :: Maybe URL
                         , feedList         :: Zipper (URL, MailboxName)
                         , emptyMailboxList :: Bool }


setMailBoxes :: MailBoxes -> TuiState -> TuiState
setMailBoxes mb ts = ts { mailBoxes = mb }

drawEditor :: TuiState -> (TuiState -> Editor T.Text ResourceName) -> Widget ResourceName
drawEditor ts editor' = visible $ renderEditor (txt . T.unlines) True (editor' ts)

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [toDraw]
    where
      toDraw = case currentDisplay ts of
        ShowEntries     -> drawMailBox ts
        ShowMailboxList -> drawMailboxesList ts
        ShowFeeds       -> drawFeedList ts
        ChooseMailbox   -> drawMailboxesList ts
        ShowArticle     -> drawArticle ts



drawArticle :: TuiState -> Widget n
drawArticle ts =
  let currEntry = getCurrent $ snd $ getCurrent $ mailBoxes ts
      art = M.fromMaybe "" (article currEntry)
  in txt art



drawFeedList :: TuiState -> Widget ResourceName
drawFeedList ts = viewport FeedsViewport Vertical
  $ vBox $ toList (addBeforeCurrent help $ addBeforeCurrent (warn ts) $ fmap (drawFeedEntry ts $ getCurrent fl) fl)
    <> [border $ drawEditor ts addFeedEditor | buttonPressed ts == Button 'n']
    where
      fl = feedList ts
      help = if showHelp ts then drawHelp ts else emptyWidget

drawFeedEntry :: TuiState -> (URL, T.Text) -> (URL, T.Text) -> Widget ResourceName
drawFeedEntry ts curFd fd = toView $ b $ border $ padRight Max $  vBox [withAttr a url, withAttr yellowAttr $ txt (snd fd)]
  where
    isCurrent = fd == curFd && (buttonPressed ts /= Button 'n')
    a :: AttrName
    a = if isCurrent then greenAttr else blueAttr
    url
      | editMode = drawEditor ts addFeedEditor
      | isCurrent && (buttonPressed ts == Button 'D') = withAttr warningAttr $ str "Press 'Y' to confirm deletion"
      | otherwise = txt $ fst fd
    toView :: Widget n -> Widget n
    toView = if isCurrent then visible else id
    editMode = isCurrent && (buttonPressed ts == Button 'e')
    b = if editMode then overrideAttr borderAttr yellowAttr else overrideAttr borderAttr defaultAttr



drawMailboxesList :: TuiState -> Widget ResourceName
drawMailboxesList ts = viewport MailboxesViewport Vertical
  $ vBox $ toList (addBeforeCurrent (warn ts) $ addBeforeCurrent help $ fmap (drawMailBoxEntry ts $ getCurrent mailboxes) mailboxes) <> [border $ drawEditor ts addMailboxEditor | buttonPressed ts == Button 'n' && currentDisplay ts == ShowMailboxList ]
    where
      mailboxes = mailBoxes ts
      help = if showHelp ts then drawHelp ts else emptyWidget


warn :: TuiState -> Widget ResourceName
warn ts = if M.isJust $ warning ts
  then overrideAttr borderAttr warningAttr $ border $ hCenter $ hLimitPercent 50 $ withAttr warningAttr (txt $ fromJust $ warning ts)
  else emptyWidget

drawMailBoxEntry :: Eq b => TuiState -> (T.Text, b) -> (T.Text, b) -> Widget ResourceName
drawMailBoxEntry ts curMb mb = a $ toView $ border $ padRight Max $ vBox [withAttr markCurrent mbName, unread]
  where
    isCurrent = mb == curMb
    markCurrent :: AttrName
    markCurrent = if (not (buttonPressed ts == Button 'n' && currentDisplay ts == ShowMailboxList)) && isCurrent then greenAttr else blueAttr
    mbName
      | editMode = drawEditor ts addMailboxEditor
      | isCurrent && (buttonPressed ts == Button 'D') = withAttr warningAttr $ str "Press 'Y' to confirm deletion"
      | otherwise = txt $ fst mb
    toView :: Widget n -> Widget n
    toView = if isCurrent then visible else id
    unread = if isCurrent && showDesc ts then str $ show $ countMatching (not . isRead) $ snd $ getCurrent $ mailBoxes ts else emptyWidget
    editMode = isCurrent && (buttonPressed ts == Button 'e')
    a = if editMode then overrideAttr borderAttr yellowAttr else overrideAttr borderAttr defaultAttr

drawMailBox :: TuiState -> Widget ResourceName
drawMailBox ts = viewport EntriesViewport Vertical
  $ vBox $ toList $ addBeforeCurrent help $ fmap (drawEntry (showDesc ts) currEnt) ents
  where
    ents      = snd $ getCurrent $ mailBoxes ts
    currEnt   = getCurrent ents
    help = if showHelp ts then drawHelp ts else emptyWidget


drawEntry :: Bool -> Entry -> Entry -> Widget n
drawEntry showDesc currEnt ent =
  toView $ overrideAttr borderAttr (if current && showDesc then greenAttr else if not $ isRead ent then blueAttr else readAttr) $ border $ padRight Max $ vBox
      [
        hBox [
              drawField (title ent) a
            , padLeft Max $ withAttr b $ drawTime (pubTime ent)
            ]
      , padRight (Pad 30) $ padTop (Pad 1) $ padBottom (Pad 1) desc
      , drawField (source ent) (if not $ isRead ent then yellowAttr else readAttr)
      ]
  where
    current = currEnt == ent
    a :: AttrName
    a = if current then greenAttr else (if not $ isRead ent then blueAttr else readAttr)
    b :: AttrName
    b = if current then greenAttr else timeAttr
    toView :: Widget n -> Widget n
    toView = if current then visible else id
    drawTime :: Maybe UTCTime -> Widget n
    drawTime Nothing  = emptyWidget
    drawTime (Just t) = str $ show t
    desc :: Widget n
    desc = if showDesc && current && hasDescription then txtWrap $ M.fromJust $ description ent else emptyWidget
    hasDescription = case description ent of
      Nothing -> False
      Just _  -> True

drawHelp :: TuiState -> Widget n
drawHelp ts = visible $ hCenterLayer $ hLimitPercent 50 $ borderWithLabel (str "help") $
              hBox
                [ padRight Max $
                    vBox [hCenter $ str $ fst x | x <- buttonDesc],
                    vBox [hCenter $ str $ snd x | x <- buttonDesc]
                ]
                where
                  buttonDesc = case (currentDisplay ts, buttonPressed ts) of
                    (ShowEntries, None)
                                            ->   [ ("q","quit")
                                                 , ("j/<down>", "nextEntry")
                                                 , ("k/<up>","prevEntry")
                                                 , ("<enter>","goToUrl")
                                                 , ("m","markAs...")
                                                 , ("r","refreshAll")
                                                 , ("g","goToTop")
                                                 , ("G","goToBottom")
                                                 , ("d","toggleDescription")
                                                 , ("?","toggleShowHelp")
                                                 , ("-","goToMailBoxes")
                                                 , ("f","goToFeeds") ]
                    (ShowEntries, Button 'm')
                                            ->   [ ("r","...read")
                                                 , ("u","...unread") ]
                    (ShowFeeds, None)
                                            ->   [ ("q","quit")
                                                 , ("j/<down>", "nextFeed")
                                                 , ("k/<up>","prevFeed")
                                                 , ("m","moveTo...")
                                                 , ("D","deleteCurrentFeed")
                                                 , ("r","refreshAll")
                                                 , ("g","goToTop")
                                                 , ("G","goToBottom")
                                                 , ("?","toggleShowHelp")
                                                 , ("-","goToMailBoxes")
                                                 , ("e","editUrl")
                                                 , ("n","newFeed")]

                    (ShowFeeds, Button 'e')  ->  [ ("<enter>","acceptChanges")
                                                 , ("<esc>","abort")]

                    (ShowFeeds, Button 'n')  ->  [ ("<enter>","addUrl -> chooseMailbox")
                                                 , ("<esc>","abort")]
                    (ShowFeeds, Button 'D')
                                             -> [ ("Y","confirm")
                                                , ("_","abort") ]

                    (ShowMailboxList, None)  ->  [ ("q","quit")
                                                 , ("j/<down>", "nextMailbox")
                                                 , ("k/<up>","prevMailbox")
                                                 , ("d","toggleShowUnread")
                                                 , ("<enter>","goToMailbox")
                                                 , ("f","goToFeeds")
                                                 , ("D","deleteMailbox")
                                                 , ("r","refreshAll")
                                                 , ("g","goToTop")
                                                 , ("G","goToBottom")
                                                 , ("?","toggleShowHelp")
                                                 , ("e","editMailboxName")
                                                 , ("n","newMailbox") ]

                    (ShowMailboxList, Button 'e')
                                             ->  [ ("<enter>","acceptChanges")
                                                 , ("<esc>","abort")]

                    (ShowMailboxList, Button 'n')
                                             ->  [ ("<enter>","createMailbox")
                                                 , ("<esc>","abort")]

                    (ShowMailboxList, Button 'D')
                                             -> [ ("Y","confirm")
                                                , ("_","abort") ]

                    (ChooseMailbox, _)       ->  [ ("<enter>","chooseMailbox")
                                                 , ("<esc>","abortOperation") ]
                    _                       -> []


drawField :: T.Text -> AttrName -> Widget n
drawField t a = withAttr a $ txt t
