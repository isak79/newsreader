module Main where

import ParseFeed (parseFeed, Entry(..))
import Brick 
-- import Brick.Widgets.Core
-- import Brick.AttrMap
-- import Brick.Types (BrickEvent(..))
import qualified Graphics.Vty as V
import qualified Data.Text as T

ui :: String -> Widget ()
ui = str

generalAttr :: AttrName
generalAttr = attrName "general"

main :: IO ()
main = do
  tuiState <- buildState
  let app = App { appAttrMap      = const $ attrMap V.defAttr [(generalAttr, fg V.blue)]
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
  pure TuiState { entries }

newtype TuiState = TuiState { entries :: [Entry] }
  deriving Show

drawTui :: TuiState -> [Widget n]
drawTui ts = [vBox $ map drawEntry $ entries ts]

drawEntry :: Entry -> Widget n
drawEntry e = withAttr generalAttr $ str $ T.unpack (title e)
