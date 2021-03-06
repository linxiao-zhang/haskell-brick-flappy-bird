{-# LANGUAGE OverloadedStrings #-}
module UI where
import System.IO
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Data.List
-- import Data.List.Split
import Snake
import qualified Brick.Main as M
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Snake| Food | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

addscorelist :: Snake.Game -> [Integer] -> Snake.Game
addscorelist g@Snake.Game{ _bird1=a,_bird2=b,_isnetwork=net,_dir =d, _dead=l, _paused=p,_score=s,_locked=m ,_food=f,_historyscore = old} h = 
  Snake.Game{ _bird1=a,_bird2=b,_isnetwork=net,_dir =d, _dead=l, _paused=p,_score=s,_locked=m ,_food=f,_historyscore = h}

split :: String -> [String] 
split [] = [""] 
split (c:cs) 
    | c == '\n' = "" : rest 
    | otherwise = (c : head rest) : tail rest 
    where rest = split cs

-- scorelist :: [Integer]
-- scorelist = [1,2,3,4,5]
writescore::Game -> IO Game
writescore g@Game { _dir = d,_score=s} = do
      let x = show s
      appendFile "/home/cse230/Desktop/test.txt" (x ++ "\n")
      return g


main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 400000 -- decides how fast your game moves
  g <- initGame 
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  -- endgame <- writescore g
  -- customMain initialVty builder (Just chan) app g
  -- endgame <- writescore g
  void $ customMain initialVty builder (Just chan) app g
  -- endgame <- writescore g
  -- void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

isConnect :: Game -> Bool
isConnect g@Game{_isnetwork = s} = if s== False then False else True

drawGame :: Game -> [Widget Name]
drawGame g@Game{_dead=d} = if d==True then [ C.center $ padRight (Pad 2) (drawStats g)] else drawUI g

drawUI :: Game -> [Widget Name]
drawUI g@Game{_isnetwork=s} = if s==True then [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ] else [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGridSingle g ]


drawStats :: Game -> Widget Name
drawStats g@Game{_dead = d} = 
  if d==False 
  then 
      hLimit 11 $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ emptyWidget
         ]
  else
    drawGameOver g
    -- drawGameOver (g ^. dead)
    
-- drawStats g = hLimit 11
--   $ vBox [ padTop (Pad 2) $ drawGameOver (g ^. dead)
--          ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Game ->  Widget Name
drawGameOver g@Game{_historyscore=history}  =  vBox $ str "   Game Over" :
                             str " Your Score is" :
                             (str <$>  ["\t" <>  (show i) | i <- history ])

-- "Line " <> 
-- listDrawElement :: (Show a) => Bool -> a -> Widget ()
-- listDrawElement sel a =
--     let selStr s = if sel
--                    then withAttr customAttr (str $ "<" <> s <> ">")
--                    else str s
--     in C.hCenter $ str "Item " <+> (selStr $ show a)
-- $ str $ show n


drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Flappy Bird")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. bird2 = Snake
      | c `elem` g ^. bird1 = Snake
      | c == g ^. food      = Food
      | otherwise           = Empty

drawGridSingle :: Game -> Widget Name
drawGridSingle g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Flappy Bird")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. bird1 = Snake
      | c == g ^. food      = Food
      | otherwise           = Empty



drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr, V.blue `on` V.blue)
  , (foodAttr, V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"
