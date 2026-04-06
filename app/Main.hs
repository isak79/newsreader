module Main where

import ParseFeed (parseFeed, Entry(..))
import Brick
-- import Brick.Widgets.Core
-- import Brick.AttrMap
-- import Brick.Types (BrickEvent(..))
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Brick.Widgets.Border
-- import Brick.Widgets.Border.Style
import System.Process (callProcess)
import Control.Monad.IO.Class (liftIO)

titleAttr, selectedTitleAttr, sourceAttr, timeAttr :: AttrName
titleAttr = attrName "title"
selectedTitleAttr = attrName "selectedTitle"
sourceAttr = attrName "source"
timeAttr = attrName "time"

main :: IO ()
main = do
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
handleTuiEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = changeEntry 1
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = changeEntry (-1)
handleTuiEvent (VtyEvent (V.EvKey V.KEnter []))      = openSelectedUrl
handleTuiEvent _                                     = pure ()

openSelectedUrl = do
  sEntry <- gets selectedEntry
  ents   <- gets entries
  let curr = ents !! sEntry 
  liftIO $ openUrl $ T.unpack $ source curr
  pure ()


openUrl :: String -> IO ()
openUrl url = callProcess "open" [url]

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)

buildState :: IO TuiState
buildState = do
  entries <- parseFeed
  pure TuiState { entries, selectedEntry = 0 }

setSelectedEntry :: Integer -> TuiState -> TuiState
setSelectedEntry i t = t { selectedEntry = i }

changeEntry :: Integer -> EventM ResourceName TuiState ()
changeEntry i = do
  sEntry <- gets selectedEntry 
  modify $ setSelectedEntry $ sEntry + i
  

data TuiState = TuiState { entries :: [Entry]
                         , selectedEntry :: Integer }
  deriving Show

drawTui :: TuiState -> [Widget n]
drawTui ts = [vBox $ map (drawEntry (selectedEntry ts)) (zip (entries ts) [0,1..] )]

drawEntry :: Eq a => a -> (Entry, a) -> Widget n
drawEntry selected (e,n) =  border $ vBox [drawField (title e) a, drawField (source e) sourceAttr]
  where 
    a :: AttrName
    a = if selected == n then selectedTitleAttr else titleAttr 
    
drawField :: T.Text -> AttrName -> Widget n
drawField t a = withAttr a $ str $ T.unpack t
