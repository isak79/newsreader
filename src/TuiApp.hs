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


blueAttr, greenAttr, sourceAttr, timeAttr, readAttr :: AttrName
readAttr = attrName "readBorder"
blueAttr = attrName "title"
greenAttr = attrName "selectedTitle"
sourceAttr = attrName "source"
timeAttr = attrName "time"

-- | The main function that ties the whole program together
runApp :: IO ()
runApp = do
  initializeTables 
  tuiState <- buildState
  let app = App { appAttrMap      = const $ attrMap V.defAttr [ (blueAttr, fg V.blue)
                                                              , (greenAttr, fg V.green)
                                                              , (sourceAttr, fg V.yellow)
                                                              , (borderAttr, fg V.white) ]
                , appStartEvent   = return ()
                , appHandleEvent  = handleTuiEvent
                , appChooseCursor = neverShowCursor
                , appDraw         = drawTui }
  _ <- defaultMain app tuiState
  pure ()

-- | The app event handler
handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent ev = do
  ts <- get
  case (currentDisplay ts,buttonPressed ts) of
    (ShowFeeds, Button 'n')       -> addFeed ev
    (ShowMailboxList, Button 'n') -> addMailbox ev
    (ShowFeeds, Button 'e')       -> renameFeed ev
    (ShowMailboxList, Button 'e') -> renameMailbox ev
    _                             -> handleNormal ev


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
        newMbs = updateCurrentItem (T.unpack mbName, snd $ getCurrent mbs) mbs
    modify $ setMailBoxes newMbs 
    modify $ setButtonPressed None
    modify (\s -> s { addMailboxEditor = editorText AddMailboxEditor (Just 1) $ T.pack "" })
    modify $ setMailBoxes newMbs 
    liftIO $ updateMailboxName (T.pack oldName) mbName
  _      -> zoom editorStateL (handleEditorEvent ev)
  where
    editorStateL = lens addMailboxEditor (\s e -> s { addMailboxEditor = e })


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
    modify $ setMailBoxes (add (T.unpack name,Zipper [] fallbackEntry []) mb)
    insertMailbox name
    modify $ setButtonPressed None
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
    modify $ setButtonPressed None
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
handleNormal (VtyEvent (V.EvKey V.KEsc []))        = modify $ setButtonPressed None
handleNormal (VtyEvent (V.EvKey (V.KChar 'r') [])) = pressR
handleNormal (VtyEvent (V.EvKey (V.KChar 'u') [])) = pressU
handleNormal (VtyEvent (V.EvKey (V.KChar 'm') [])) = modify $ setButtonPressed (Button 'm')
handleNormal (VtyEvent (V.EvKey (V.KChar 'n') [])) = modify $ setButtonPressed (Button 'n')
handleNormal (VtyEvent (V.EvKey (V.KChar 'f') [])) = modify $ setCurrentDisplay ShowFeeds
handleNormal (VtyEvent (V.EvKey (V.KChar 'e') [])) = do
  ts <- get
  case currentDisplay ts of
    ShowFeeds -> do
      let url = fst $ getCurrent $ feedList ts
      modify (\s -> s { addFeedEditor = editorText AddFeedEditor (Just 1) url })
      modify $ setButtonPressed $ Button 'e'
    ShowMailboxList -> do
      let mbName = fst $ getCurrent $ mailBoxes ts
      modify (\s -> s { addMailboxEditor = editorText AddMailboxEditor (Just 1) $ T.pack mbName })
      modify $ setButtonPressed $ Button 'e'
      pure ()
    _         -> pure ()
handleNormal _ = pure ()

-- | Fetches every feeed user is currently subscribed to, updates the database, and loads everything into memory
refillMailboxes :: EventM ResourceName TuiState ()
refillMailboxes = do
  liftIO refreshAll
  mb <- liftIO fillMailboxes
  modify $ setMailBoxes mb

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
  case curDisplay of
     ShowMailboxList  -> modify $ setCurrentDisplay ShowEntries
     ChooseMailbox    -> do
        mb <- gets mailBoxes
        let currMb = getCurrent mb
        url <- gets newFeedUrl
        addFeedToMailbox (M.fromJust url) $ T.pack $ fst currMb
        modify $ setCurrentDisplay ShowFeeds
        modify $ setNewFeedUrl Nothing
        feed <- liftIO safeFeeds 
        modify $ setFeedList $ M.fromJust $ fromList feed
     ShowFeeds -> pure ()
     _    -> openSelectedUrl

-- | Update the states display element
setCurrentDisplay :: CurrentDisplay -> TuiState -> TuiState
setCurrentDisplay cd ts = ts { currentDisplay = cd }

-- | Open the current enties URL
openSelectedUrl :: EventM ResourceName TuiState ()
openSelectedUrl = do
  mailBoxes   <- gets mailBoxes
  let currEntry = getCurrent $ snd $ getCurrent mailBoxes
  markEntry True
  liftIO $ openUrl $ T.unpack $ source currEntry
  pure ()

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
  let safe = if null feeds then [(T.pack "No url", T.pack "No feed")] else feeds
  pure safe

-- | Build the initial TUI state
buildState :: IO TuiState
buildState = do
  mailboxes <- fillMailboxes
  feeds     <- safeFeeds 
  pure TuiState { currentDisplay   = ShowMailboxList
                , showDesc         = False
                , showHelp         = False
                , mailBoxes        = mailboxes
                , buttonPressed    = None
                , addFeedEditor    = editorText AddFeedEditor (Just 1) $ T.pack ""
                , addMailboxEditor = editorText AddMailboxEditor (Just 1) $ T.pack ""
                , feedList         = M.fromJust $ fromList feeds
                , newFeedUrl       = Nothing }

-- | Fetch every feed, update database and memory
fillMailboxes :: IO MailBoxes
fillMailboxes = do
  mbs <- fetchMailboxes
  mailboxes0 <- if null mbs 
      then pure [("Empty mailbox", Zipper [] fallbackEntry [])]
      else traverse mkMailbox mbs
  let mailboxes = M.fromJust $ fromList mailboxes0
  pure mailboxes
      where
        mkMailbox mbName = do
          e <- fetchEntries mbName
          let mb = maybe (Zipper [] fallbackEntry []) id (fromList $ entrySort e)
          pure (T.unpack mbName, mb)
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
  deriving (Show, Functor, Eq)

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


type MailboxName = String
type Mailbox     = Zipper Entry
type MailBoxes   = Zipper (MailboxName, Mailbox)

data CurrentDisplay = ShowMailboxList | ShowEntries | ShowFeeds | ChooseMailbox
  deriving Eq

data ButtonPressed x = Button x | None
  deriving Eq

data TuiState = TuiState { currentDisplay   :: CurrentDisplay
                         , showDesc         :: Bool
                         , showHelp         :: Bool
                         , mailBoxes        :: MailBoxes
                         , buttonPressed    :: ButtonPressed Char
                         , addFeedEditor    :: Editor T.Text ResourceName
                         , addMailboxEditor :: Editor T.Text ResourceName
                         , newFeedUrl       :: Maybe URL
                         , feedList         :: Zipper (URL, T.Text) }


setMailBoxes :: MailBoxes -> TuiState -> TuiState
setMailBoxes mb ts = ts { mailBoxes = mb }

drawEditor :: TuiState -> (TuiState -> Editor T.Text ResourceName) -> Widget ResourceName
drawEditor ts editor' = renderEditor (txt . T.unlines) True (editor' ts)

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts
  | showHelp ts = [drawHelp ts, toDraw]
  | otherwise   = [toDraw]
    where
      toDraw = case currentDisplay ts of
        ShowEntries     -> drawMailBox ts
        ShowMailboxList -> drawHome ts
        ShowFeeds       -> drawFeedList ts
        ChooseMailbox   -> drawHome ts

drawFeedList :: TuiState -> Widget ResourceName
drawFeedList ts = viewport FeedsViewport Vertical
  $ vBox $ toList (fmap (drawFeedEntry ts $ getCurrent fl) fl) <> [border $ drawEditor ts addFeedEditor | buttonPressed ts == Button 'n']
    where
      fl = feedList ts

drawFeedEntry :: TuiState -> (URL, T.Text) -> (URL, T.Text) -> Widget ResourceName
drawFeedEntry ts curFd fd = border $ padRight Max $  vBox [withAttr a url, withAttr sourceAttr $ txt (snd fd)]
  where
    isCurrent = fd == curFd
    a :: AttrName
    a = if isCurrent then greenAttr else blueAttr
    url = if isCurrent && (buttonPressed ts == Button 'e') then drawEditor ts addFeedEditor else txt $ fst fd


drawHome :: TuiState -> Widget ResourceName
drawHome ts = viewport MailboxesViewport Vertical
  $ vBox $ toList (fmap (drawMailBoxEntry ts $ getCurrent mailboxes) mailboxes) <> [border $ drawEditor ts addMailboxEditor | buttonPressed ts == Button 'n']
    where 
      mailboxes = mailBoxes ts

drawMailBoxEntry :: Eq b => TuiState -> (String, b) -> (String, b) -> Widget ResourceName
drawMailBoxEntry ts curMb mb = border $ padRight Max $ withAttr a mbName
  where
    isCurrent = mb == curMb
    a :: AttrName
    a = if isCurrent then greenAttr else blueAttr
    mbName = if isCurrent && (buttonPressed ts == Button 'e') then drawEditor ts addMailboxEditor else txt $ T.pack $ fst mb

drawMailBox :: TuiState -> Widget ResourceName
drawMailBox ts = viewport EntriesViewport Vertical
  $ vBox $ toList $ fmap (drawEntry (showDesc ts) currEnt) ents
  where
    ents      = snd $ getCurrent $ mailBoxes ts
    currEnt   = getCurrent ents

drawEntry :: Bool -> Entry -> Entry -> Widget n
drawEntry showDesc currEnt ent =
  toView $ overrideAttr borderAttr (if current && showDesc then greenAttr else if not $ isRead ent then blueAttr else readAttr) $ border $ padRight Max $ vBox
      [
        hBox [
              drawField (title ent) a
            , padLeft Max $ withAttr b $ drawTime (pubTime ent)
            ]
      , padRight (Pad 30) $ padTop (Pad 1) $ padBottom (Pad 1) desc
      , drawField (source ent) (if not $ isRead ent then sourceAttr else readAttr)
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
drawHelp ts =  hCenterLayer $ hLimitPercent 50 $ borderWithLabel (str "help") $
              hBox
                [ padRight Max $
                    vBox [hCenter $ str x | x <- buttons],
                    vBox [hCenter $ str x | x <- descriptions]
                ]
                where
                  goTo = if box then "goToUrl" else "goToMailbox"
                  nextItem' = if box then "nextEntry" else "nextMailbox"
                  prevItem' = if box then "prevEntry" else "prevMailbox"
                  buttons = case bp of
                    Button 'm' -> ["r","u"]
                    _          -> ["q","j/<down>","k/<up>","<enter>"] <> ["m" | box]<> ["r","g","G","d","?","-"]
                  descriptions = case bp of
                    Button 'm' -> ["read", "unread"]
                    _          -> ["exitApp",nextItem',prevItem',goTo] <> ["markAs..."| box] <> ["refreshAll","goToTop","goToBottom","toggleDescription","toggleHelp","goToMailboxList"]
                  box = currentDisplay ts == ShowMailboxList
                  bp  = buttonPressed ts


drawField :: T.Text -> AttrName -> Widget n
drawField t a = withAttr a $ txt t
