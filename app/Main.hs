module Main where

import ParseFeed (parseFeed, Entry)
import Brick 
import Brick.Widgets.Core
import Brick.AttrMap
import Brick.Types (BrickEvent(..))
import qualified Graphics.Vty as V

ui :: String -> Widget ()
ui = str

main :: IO ()
main = do
  tuiState <- buildState
  let generalAttr = attrName "general"
  let app = App { appAttrMap      = const $ attrMap V.defAttr [(generalAttr, fg V.blue)]
                , appStartEvent   = return ()
                , appHandleEvent  = handleTuiEvent 
                , appChooseCursor = neverShowCursor
                , appDraw         = drawTui }
  _ <- defaultMain app tuiState 
  pure () 

handleTuiEvent :: BrickEvent ResourceName e -> EventM ResourceName TuiState ()
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleTuiEvent (VtyEvent (V.EvKey V.KEsc []))       = halt
handleTuiEvent _                                    = pure ()

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)

buildState = do
  entries <- parseFeed
  pure TuiState { entries }

newtype TuiState = TuiState { entries :: [Entry] }
  deriving Show

drawTui ts = [vBox $ map drawEntry $ entries ts]

drawEntry :: Entry -> Widget n
drawEntry e = str $ show e
