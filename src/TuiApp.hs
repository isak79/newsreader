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


blueAttr, greenAttr, sourceAttr, timeAttr, readAttr, cyanAttr :: AttrName
cyanAttr = attrName "cyanAttr"
readAttr = attrName "readBorder"
blueAttr = attrName "title"
greenAttr = attrName "selectedTitle"
sourceAttr = attrName "source"
timeAttr = attrName "time"

runApp :: IO ()
runApp = do
  tuiState <- buildState
  let app = App { appAttrMap      = const $ attrMap V.defAttr [ (blueAttr, fg V.blue)
                                                              , (greenAttr, fg V.green)
                                                              , (sourceAttr, fg V.yellow) 
                                                              , (borderAttr, fg V.white) 
                                                              , (cyanAttr, fg V.green) ]
                , appStartEvent   = return ()
                , appHandleEvent  = handleTuiEvent
                , appChooseCursor = neverShowCursor
                , appDraw         = drawTui }
  _ <- defaultMain app tuiState
  pure ()

handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent ev = do
  ts <- get
  case buttonPressed ts of
    Button 'e' -> handleEdit ev
    _          -> handleNormal ev


handleEdit :: BrickEvent ResourceName e -> EventM ResourceName TuiState () 
handleEdit ev = case ev of
  (VtyEvent (V.EvKey V.KEsc [])) -> modify $ setButtonPressed None
  _                              -> zoom editorStateL (handleEditorEvent ev)
  where
    editorStateL = lens editorState (\s e -> s { editorState = e })

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
handleNormal (VtyEvent (V.EvKey (V.KChar 'e') [])) = modify $ setButtonPressed (Button 'e')
handleNormal _                                     = pure ()

refillMailboxes :: EventM ResourceName TuiState ()
refillMailboxes = do
  liftIO refreshAll 
  mb <- liftIO fillMailboxes 
  modify $ setMailBoxes mb

pressU :: EventM ResourceName TuiState ()
pressU = do
  bp <- gets buttonPressed 
  case bp of
    Button 'm' -> do
      markEntry False
      modify $ setButtonPressed None
    _ -> return ()

pressR :: EventM ResourceName TuiState ()
pressR = do
  bp <- gets buttonPressed 
  case bp of
    Button 'm' -> do
      markEntry True
      modify $ setButtonPressed None
    _       -> refillMailboxes 

setButtonPressed :: ButtonPressed Char -> TuiState -> TuiState
setButtonPressed c t = t { buttonPressed = c }


data Dir = Next | Prev | Top | Bottom
  deriving Eq

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
    ShowMailboxList
              -> do
                let newMb = func mb
                modify $ setMailBoxes newMb
    where
      func = case switchTo of
        Next   -> nextItem 
        Prev   -> prevItem 
        Top    -> firstItem
        Bottom -> lastItem

  


activateItem :: EventM ResourceName TuiState ()
activateItem = do
  curDisplay    <- gets currentDisplay
  case curDisplay of
     ShowMailboxList  -> modify $ setCurrentDisplay ShowEntries
     _    -> openSelectedUrl 

setCurrentDisplay :: CurrentDisplay -> TuiState -> TuiState
setCurrentDisplay cd ts = ts { currentDisplay = cd } 

openSelectedUrl :: EventM ResourceName TuiState ()
openSelectedUrl = do
  mailBoxes   <- gets mailBoxes
  let currEntry = getCurrent $ snd $ getCurrent mailBoxes 
  markEntry True
  liftIO $ openUrl $ T.unpack $ source currEntry
  pure ()

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

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)

toggleShowHelp :: EventM ResourceName TuiState ()
toggleShowHelp = do
  sh <- gets showHelp
  modify $ setShowHelp $ not sh

setShowHelp :: Bool -> TuiState -> TuiState
setShowHelp b t = t { showHelp = b}

buildState :: IO TuiState
buildState = do
  mailboxes <- fillMailboxes 
  pure TuiState { currentDisplay = ShowMailboxList
                , showDesc       = False 
                , showHelp       = False 
                , mailBoxes      = mailboxes
                , buttonPressed  = None 
                , editorState    = editorText ResourceName Nothing $ T.pack "" }

fillMailboxes :: IO MailBoxes
fillMailboxes = do
  mbs <- fetchMailboxes 
  mailboxes0 <- traverse mkMailbox mbs
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

-- onTop :: Zipper a -> Bool
-- onTop (Zipper xs _ _) = null xs

updateCurrentItem :: a -> Zipper a -> Zipper a
updateCurrentItem z (Zipper xs _ zs) = Zipper xs z zs


type MailboxName = String
type Mailbox     = Zipper Entry
type MailBoxes   = Zipper (MailboxName, Mailbox)

data CurrentDisplay = ShowMailboxList | ShowEntries

data ButtonPressed x = Button x | None

data TuiState = TuiState { currentDisplay :: CurrentDisplay
                         , showDesc       :: Bool 
                         , showHelp       :: Bool 
                         , mailBoxes      :: MailBoxes
                         , buttonPressed  :: ButtonPressed Char 
                         , editorState    :: Editor T.Text ResourceName }


setMailBoxes :: MailBoxes -> TuiState -> TuiState
setMailBoxes mb ts = ts { mailBoxes = mb }

drawEditor ts = renderEditor (txt . T.unlines) True (editorState ts)

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts 
  | showHelp ts = [drawHelp (buttonPressed ts) inBox , toDraw]
  | otherwise   = [border $ drawEditor ts | edit] <> [toDraw]
    where
      inBox = case currentDisplay ts of
        ShowMailboxList -> False
        _               -> True
      toDraw = if inBox then drawMailBox ts else drawHome $ mailBoxes ts
      edit   = case buttonPressed ts of
        Button 'e' -> True
        _   -> False


drawHome :: Eq b => Zipper (String, b) -> Widget n
drawHome mailboxes = vBox $ toList $ fmap (drawMailBoxEntry $ getCurrent mailboxes) mailboxes 


drawMailBoxEntry :: Eq b => (String, b) -> (String, b) -> Widget n
drawMailBoxEntry curMb mb = border $ padRight Max $ withAttr a $ str $ fst mb
  where 
    isCurrent = mb == curMb
    a :: AttrName
    a = if isCurrent then greenAttr else blueAttr 


drawMailBox :: TuiState -> Widget ResourceName
drawMailBox ts = viewport ResourceName Vertical 
  $ vBox $  toList $ fmap (drawEntry (showDesc ts) currEnt) ents
  where
    -- makeVisib = if  onTop ents then visible else id 
    ents      = snd $ getCurrent $ mailBoxes ts
    currEnt   = getCurrent ents


drawEntry :: Bool -> Entry -> Entry -> Widget n
drawEntry showDesc currEnt ent =  
  toView $ overrideAttr borderAttr (if current && showDesc then cyanAttr else if not $ isRead ent then blueAttr else readAttr) $ border $ padRight Max $ vBox 
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

drawHelp :: ButtonPressed Char -> Bool -> Widget n
drawHelp bp box =  hCenterLayer $ hLimitPercent 50 $ borderWithLabel (str "help") $ 
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

drawField :: T.Text -> AttrName -> Widget n
drawField t a = withAttr a $ txt t
