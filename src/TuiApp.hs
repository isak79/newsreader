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
      selectedItem <- gets selectedItem 
      changeInMailBox (Box (mailBoxes !! selectedItem))
    _    -> openSelectedUrl 

openSelectedUrl :: EventM ResourceName TuiState ()
openSelectedUrl = do
  sEntry <- gets selectedItem
  ents   <- gets entries
  let curr = ents !! sEntry 
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
  entries <- parseFeed "https://www.vg.no/rss/feed/?format=rss"
  pure TuiState { entries       = entries
                , selectedItem  = 0
                , showDesc      = False 
                , showHelp      = False 
                , inMailbox     = None 
                , mailBoxes     = ["VG", "NYT"] }

setSelectedItem :: Int -> TuiState -> TuiState
setSelectedItem i t = t { selectedItem = i }

switchItem :: Int -> EventM ResourceName TuiState ()
switchItem i = do
  sEntry    <- gets selectedItem 
  entries   <- gets entries
  mailBoxes <- gets mailBoxes
  inMailbox <- gets inMailbox 
  let nrItems = if (inMailbox == None) then length mailBoxes else length entries
  modify $ setSelectedItem $ if (sEntry + i) < nrItems && (sEntry + i) >= 0 then sEntry + i else sEntry 

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

data TuiState = TuiState { entries       :: [Entry]
                         , selectedItem  :: Int 
                         , showDesc      :: Bool 
                         , showHelp      :: Bool 
                         , inMailbox     :: MailBox String 
                         , mailBoxes     :: [String] }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts 
  | showHelp ts = [drawHelp, drawMain ts]
  | otherwise   = [drawMain ts]
      

drawMain :: TuiState -> Widget ResourceName
drawMain ts = case (inMailbox ts) of
  None  -> drawHome ts
  Box _ -> drawMailBox ts

drawHome :: TuiState -> Widget ResourceName
drawHome ts = vBox $ map (drawMailBoxEntry (selectedItem ts)) (zip (mailBoxes ts) [0,1..])


drawMailBoxEntry :: Eq b => b -> (String, b) -> Widget n
drawMailBoxEntry selected (st,n) = border $ padRight Max $ withAttr a $ str st
  where 
    current = selected == n
    a :: AttrName
    a = if current then selectedTitleAttr else titleAttr 


drawMailBox :: TuiState -> Widget ResourceName
drawMailBox ts = viewport ResourceName Vertical $ borderWithLabel (makeVisible $ str $ mailBoxLabel (inMailbox ts)) $ vBox $ map (drawEntry (showDesc ts) (selectedItem ts)) (zip (entries ts) [0,1..])
  where
    makeVisible = if (selectedItem ts) == 0 then visible else id 

drawHelp :: Widget n
drawHelp =  hCenterLayer $ hLimitPercent 50 $ borderWithLabel (str "help") $ 
              hBox 
                [padRight Max $ 
                    vBox [hCenter $ str x | x <- ["g", "G", "<enter>", "j", "k", "d", "?"]], 
                    vBox [hCenter $ str x | x <- ["goToTop","goToBottom","goToSource","nextEntry","prevEntry","toggleDescription","toggleHelp"]]
                ]

drawEntry :: Eq b => Bool -> b -> (Entry, b) -> Widget n
drawEntry showDesc selected (e,n) =  
  toView $ padRight Max $ vBox 
      [
        hBox [
              drawField (title e) a 
            , padLeft Max $ withAttr b $ drawTime (pubTime e) 
            ]
      , padRight (Pad 30) $ padTop (Pad 1) $ padBottom (Pad 1) desc
      , drawField (source e) sourceAttr
      ]
  where 
    current = selected == n
    a :: AttrName
    a = if current then selectedTitleAttr else titleAttr 
    b :: AttrName
    b = if current then selectedTitleAttr else timeAttr
    toView :: Widget n -> Widget n
    toView = if current then visible . border else border
    drawTime :: Maybe UTCTime -> Widget n
    drawTime Nothing  = emptyWidget 
    drawTime (Just t) = str $ show t
    desc :: Widget n
    desc = if showDesc && current && hasDescription then txtWrap $ fromJust $ description e else emptyWidget 
    hasDescription = case description e of
      Nothing -> False
      Just _  -> True

drawField :: T.Text -> AttrName -> Widget n
drawField t a = withAttr a $ txt t
