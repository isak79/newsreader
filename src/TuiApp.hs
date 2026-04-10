-- module TuiApp(runApp) where
--
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
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = switchToNextItem
-- handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = switchToPrev
handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = changeShowDesc
-- handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = goToTop 
-- handleTuiEvent (VtyEvent (V.EvKey (V.KChar 'G') [])) = goToBottom 
handleTuiEvent (VtyEvent (V.EvKey (V.KChar '?') [])) = toggleShowHelp 
handleTuiEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = modify $ setCurrentDisplay ShowMailboxList
handleTuiEvent (VtyEvent (V.EvKey V.KEnter []))      = activateItem 
handleTuiEvent _                                     = pure ()


switchToNextItem = do
  mb <- gets mailBoxes
  cd <- gets currentDisplay 
  case cd of
    ShowEntries 
              -> do   
                let (curMailBoxName, curMailBox)   = getCurrent mb
                let newMailBox   = nextItem curMailBox 
                let newMailBoxes = updateCurrentItem (curMailBoxName,newMailBox) mb 
                modify $ setMailBoxes newMailBoxes 
    ShowMailboxList
              -> do
                let newMb = nextItem mb
                modify $ setMailBoxes newMb

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
  liftIO $ openUrl $ T.unpack $ source currEntry
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
  entriesVG   <- parseFeed "https://www.vg.no/rss/feed/?format=rss"
  entriesNYT  <- parseFeed "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml"
  pure TuiState { currentDisplay = ShowMailboxList
                , showDesc       = False 
                , showHelp       = False 
                , mailBoxes      = fromJust $ fromList [
                  ("VG", fromJust $ fromList entriesVG)
                , ("NYT", fromJust $ fromList entriesNYT)
                ] }

-- setMailBox :: (MailboxName, Zipper Entry) -> TuiState -> TuiState
setMailBox mb ts = ts { mailBoxes = mb }


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
    (h,r) = fromJust $ unsnoc xs


lastItem :: Zipper a -> Zipper a
lastItem z@(Zipper _ _ []) = z
lastItem (Zipper xs y zs)  = Zipper (reverse h ++ y:xs) r []
  where
    (h,r) = fromJust $ unsnoc zs


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

updateCurrentItem :: a -> Zipper a -> Zipper a
updateCurrentItem z (Zipper xs y zs) = Zipper xs z zs


type MailboxName = String
type Mailbox     = Zipper Entry
type MailBoxes   = Zipper (MailboxName, Mailbox)

data CurrentDisplay = ShowMailboxList | ShowEntries

data TuiState = TuiState { currentDisplay :: CurrentDisplay
                         , showDesc       :: Bool 
                         , showHelp       :: Bool 
                         , mailBoxes      :: MailBoxes  }


setMailBoxes :: MailBoxes -> TuiState -> TuiState
setMailBoxes mb ts = ts { mailBoxes = mb }


drawTui :: TuiState -> [Widget ResourceName]
drawTui ts 
  | showHelp ts = [drawHelp inBox , toDraw]
  | otherwise   = [toDraw]
    where
      inBox = case currentDisplay ts of
        ShowMailboxList -> False
        _               -> True
      toDraw = if inBox then drawMailBox ts else drawHome $ mailBoxes ts


-- drawHome :: TuiState -> Widget ResourceName
drawHome mailboxes = vBox $ toList $ fmap (drawMailBoxEntry $ getCurrent mailboxes) mailboxes 


-- drawMailBoxEntry :: Eq b => b -> (String, b) -> Widget n
drawMailBoxEntry mb curMb = border $ padRight Max $ withAttr a $ str $ fst mb
  where 
    isCurrent = mb == curMb
    a :: AttrName
    a = if isCurrent then selectedTitleAttr else titleAttr 


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
