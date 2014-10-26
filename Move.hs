module Move where

import Position
import State
import Map
import Player
import Control.Monad
import Data.IORef
import System.IO

canMoveTo :: GameMap -> Position -> Bool
canMoveTo _ (x, y) 
    | x < 0 = False
    | y < 0 = False
    | x > 100 = False
    | y > 100 = False
canMoveTo m p = not $ isWallAt m p

moveRel :: Position -> IO ()
moveRel (x, y) = do
    addLog $ "Move " ++ (show (x, y))
    map <- readIORef State.map
    p <- readIORef player
    let (px, py) = position p
    let newPos = (px + x, py + y)
    when (canMoveTo map newPos) $ do
        let newPlayer = p { pPosition=newPos }
        writeIORef player newPlayer

