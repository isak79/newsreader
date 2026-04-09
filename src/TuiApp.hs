module TuiApp(runApp) where

import ParseFeed (parseFeed, Entry(..))
import Brick
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Brick.Widgets.Border
import System.Process (callProcess)
import Control.Monad.IO.Class (liftIO)
import System.Info
import Data.Time (UTCTime)
import Data.Maybe (fromJust)
import Brick.Widgets.Center (hCenter, hCenterLayer)

titleAttr, selectedTitleAttr, sourceAttr, timeAttr :: AttrName
titleAttr = attrName "title"
selectedTitleAttr = attrName "selectedTitle"
sourceAttr = attrName "source"
timeAttr = attrName "time"

runApp :: IO ()
runApp = do
  tuiState <- buildState
  let app = App { appAttrMap      = const $ attrMap V.defAttr [ (titleAttr, fg V.blue)
                                                              , (selectedTitleAttr, fg V.green)
                                                              , (sourceAttr, fg V.yellow) ]
                , appStartEvent   = return ()
                , appHandleEvent  = handleTuiEvent
                , appChooseCursor = neverShowCursor
                , appDraw         = drawTui }
  _ <- defaultMain app tuiState
  pure ()

handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = switchItem 1
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = switchItem (-1)
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = changeShowDesc
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = goToTop 
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'G') [])) = goToBottom 
handleTuiEvent (VtyEvent (V.EvKey (V.KChar '?') [])) = toggleShowHelp 
handleTuiEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = changeInMailBox None
handleTuiEvent (VtyEvent (V.EvKey V.KEnter []))      = activateItem 
handleTuiEvent _                                     = pure ()

setInMailbox :: MailBox String -> TuiState -> TuiState
setInMailbox mb ts = ts { inMailbox = mb }

changeInMailBox :: MailBox String -> EventM ResourceName TuiState ()
changeInMailBox mb = do
  modify $ setInMailbox mb

activateItem :: EventM ResourceName TuiState ()
activateItem = do
  inMailbox    <- gets inMailbox
  case inMailbox of
    None -> do
      mailBoxes    <- gets mailBoxes 
      changeInMailBox (Box (getCurrent mailBoxes))
    _    -> openSelectedUrl 

openSelectedUrl :: EventM ResourceName TuiState ()
openSelectedUrl = do
  ents   <- gets entries
  let curr = getCurrent ents
  liftIO $ openUrl $ T.unpack $ source curr
  pure ()


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
  entries0 <- parseFeed "https://www.vg.no/rss/feed/?format=rss"
  let entries = fromJust $ fromList entries0 
  pure TuiState { entries       = entries
                , selectedItem  = 0
                , showDesc      = False 
                , showHelp      = False 
                , inMailbox     = None 
                , mailBoxes     = fromJust $ fromList ["VG", "NYT"] }

setSelectedItem :: Int -> TuiState -> TuiState
setSelectedItem i t = t { selectedItem = i }

setMailBoxes :: Zipper String -> TuiState -> TuiState
setMailBoxes mb ts = ts { mailBoxes = mb }

switchItem :: Int -> EventM ResourceName TuiState ()
switchItem i = do
  inMailbox <- gets inMailbox 
  case inMailbox of
    None
      -> do
        mailBoxes <- gets mailBoxes 
        let newMailBoxes = nextItem mailBoxes 
        modify $ setMailBoxes newMailBoxes 

goToTop :: EventM ResourceName TuiState ()
goToTop = do
  modify $ setSelectedItem 0


goToBottom :: EventM ResourceName TuiState ()
goToBottom = do
  entries <- gets entries
  modify $ setSelectedItem $ length entries - 1

setShowDesc :: Bool -> TuiState -> TuiState
setShowDesc b t = t { showDesc = b }


changeShowDesc :: EventM ResourceName TuiState ()
changeShowDesc = do
  prev <- gets showDesc 
  modify $ setShowDesc $ not prev
  

data MailBox x = Box x | None
  deriving Eq

mailBoxLabel :: MailBox String -> String
mailBoxLabel None    = "None"
mailBoxLabel (Box v) = "MailBox " ++ v

data Zipper a = Zipper [a] a [a]
  deriving (Show, Functor)

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

onTop :: Zipper a -> Bool
onTop (Zipper xs _ _) = null xs

data TuiState = TuiState { entries       :: Zipper Entry
                         , selectedItem  :: Int 
                         , showDesc      :: Bool 
                         , showHelp      :: Bool 
                         , inMailbox     :: MailBox String 
                         , mailBoxes     :: Zipper String }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts 
  | showHelp ts = [drawHelp box , toDraw]
  | otherwise   = [toDraw]
    where
      box = case inMailbox ts of
        Box _ -> True
        _     -> False
      toDraw = if box then drawMailBox ts else drawHome (mailBoxes ts)
      

-- drawHome :: TuiState -> Widget ResourceName
drawHome mailboxes = vBox $ toList $ fmap (drawMailBoxEntry $ getCurrent mailboxes) mailboxes 


-- drawMailBoxEntry :: Eq b => b -> (String, b) -> Widget n
drawMailBoxEntry mb curMb = border $ padRight Max $ withAttr a $ str mb
  where 
    current = mb == curMb
    a :: AttrName
    a = if current then selectedTitleAttr else titleAttr 


drawMailBox :: TuiState -> Widget ResourceName
drawMailBox ts = viewport ResourceName Vertical 
  $ vBox $  toList $ fmap (drawEntry (showDesc ts) currEnt) ents
  where
    makeVisib = if  onTop (entries ts) then visible else id 
    ents      = entries ts
    currEnt   = getCurrent ents


-- drawEntry :: Eq b => Bool -> b -> (Entry, b) -> Widget n
drawEntry showDesc currEnt ent =  
  toView $ border $ padRight Max $ vBox 
      [
        hBox [
              drawField (title ent) a 
            , padLeft Max $ withAttr b $ drawTime (pubTime ent) 
            ]
      , padRight (Pad 30) $ padTop (Pad 1) $ padBottom (Pad 1) desc
      , drawField (source ent) sourceAttr
      ]
  where 
    current = currEnt == ent
    a :: AttrName
    a = if current then selectedTitleAttr else titleAttr 
    b :: AttrName
    b = if current then selectedTitleAttr else timeAttr
    toView :: Widget n -> Widget n
    toView = if current then visible else id
    drawTime :: Maybe UTCTime -> Widget n
    drawTime Nothing  = emptyWidget 
    drawTime (Just t) = str $ show t
    desc :: Widget n
    desc = if showDesc && current && hasDescription then txtWrap $ fromJust $ description ent else emptyWidget 
    hasDescription = case description ent of
      Nothing -> False
      Just _  -> True

drawHelp :: Bool -> Widget n
drawHelp box =  hCenterLayer $ hLimitPercent 50 $ borderWithLabel (str "help") $ 
              hBox 
                [padRight Max $ 
                    vBox [hCenter $ str x | x <- ["g", "G", "<enter>", "j", "k", "d", "?"]], 
                    vBox [hCenter $ str x | x <- ["goToTop","goToBottom", goTo,"nextEntry","prevEntry","toggleDescription","toggleHelp"]]
                ]
                where
                  goTo = if box then "goToUrl" else "goToMailBox"

drawField :: T.Text -> AttrName -> Widget n
drawField t a = withAttr a $ txt t
