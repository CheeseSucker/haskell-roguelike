module State where

import Player
import Map
import Data.IORef
import System.IO
import System.IO.Unsafe
import Monster

_defaultPlayer = Player "PlayerName" (3, 5) 100 3

player :: IORef Player
player = unsafePerformIO (newIORef _defaultPlayer)

map :: IORef GameMap
map = unsafePerformIO (newIORef [])

monsters :: IORef [Monster]
monsters = unsafePerformIO (newIORef [])

gameLog :: IORef [String]
gameLog = unsafePerformIO (newIORef [])

addLog :: String -> IO ()
addLog s = do
    l <- readIORef gameLog
    writeIORef gameLog (s:l)
    return ()
