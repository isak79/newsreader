module Main where

import ParseFeed (parseFeed, Entry(..))
import Brick
-- import Brick.Widgets.Core
-- import Brick.AttrMap
-- import Brick.Types (BrickEvent(..))
import qualified Graphics.Vty as V
import qualified Data.Text as T
import Brick.Widgets.Border

ui :: String -> Widget ()
ui = str

titleAttr, sourceAttr, timeAttr :: AttrName
titleAttr = attrName "title"
sourceAttr = attrName "source"
timeAttr = attrName "time"

main :: IO ()
main = do
  tuiState <- buildState
  let app = App { appAttrMap      = const $ attrMap V.defAttr [(titleAttr, fg V.blue), (sourceAttr, fg V.yellow)]
                , appStartEvent   = return ()
                , appHandleEvent  = handleTuiEvent
                , appChooseCursor = neverShowCursor
                , appDraw         = drawTui }
  _ <- defaultMain app tuiState
  pure ()

handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleTuiEvent (VtyEvent (V.EvKey V.KEsc []))        = halt
handleTuiEvent _                                     = pure ()

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)

buildState :: IO TuiState
buildState = do
  entries <- parseFeed
  pure TuiState { entries, selectedEntry = 1 }

setSelectedEntry :: Integer -> TuiState -> TuiState
setSelectedEntry i t = t { selectedEntry = i }

-- nextEntry :: State TuiState ()
nextEntry t = do
  sEntry <- gets selectedEntry 
  modify $ setSelectedEntry $ sEntry + 1
  

data TuiState = TuiState { entries :: [Entry]
                         , selectedEntry :: Integer }
  deriving Show

drawTui :: TuiState -> [Widget n]
drawTui ts = [vBox $ map drawEntry $ entries ts]

drawEntry :: Entry -> Widget n
drawEntry e = border $ vBox [drawField (title e) titleAttr, drawField (source e) sourceAttr]

drawField :: T.Text -> AttrName -> Widget n
drawField t a = withAttr a $ str $ T.unpack t
