module Main where

import Position
import Data.IORef
import Data.Char
import Control.Concurrent
import Control.Monad
import TerminalStuff
import Map
import Move
import Monster
import Player
import State
import Ray
import Data.Maybe
import System.IO
import System.IO.Unsafe
import Vision
import Vision.DDA
-- import UI.NCurses

----- HACK TO MAKE SETBUFFERING WORK ON WINDOWS! ----
{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
---------- END HACK ---------------------

-- TODO: Use proper state. This is frowned upon!
shouldQuit :: IORef Bool
{-# NOINLINE shouldQuit #-}
shouldQuit = unsafePerformIO (newIORef False)

display_map :: GameMap -> IO ()
display_map m =    do
    mapM_ putStrLn m

drawPlayer :: Position -> IO ()
drawPlayer (x, y) = do
    setCursor (x+1) (y+1)
    putStr $ colorChr '@'
    putStr clrReset

drawMonsters :: [Monster] -> IO ()
drawMonsters [] = return ()
drawMonsters (m:ms) = do
    drawMonster m
    drawMonsters ms

drawMonster :: Monster -> IO ()
drawMonster m = do
    map <- readIORef State.map
    p <- readIORef State.player
    when (isVisible map (position p) (position m)) $ do
        let (x, y) = position m
        setCursor (x+1) (y+1)
        putStr $ colorChr 'X'
        putStr clrReset

printSeparator :: IO ()
printSeparator = do
    w <- consoleWidth
    let s = "="
    putStr $ concat [s | x <- [0..w]]

printStat :: Show a => Position -> String -> a -> IO ()
printStat (x, y) title var = do
    setCursor x y
    putStr $ title ++ ": "
    putStr $ show var

drawHud :: Position -> IO ()
drawHud pos = do
    h <- consoleHeight
    let top = h - 1
    setCursor 0 top
    printSeparator

    p <- readIORef State.player
    printStat (0, top + 1) "Pos" $ position p
    printStat (20, top + 1) "HP" $ health p

    setCursor 40 (top - 2)
    drawLog

drawLog :: IO ()
drawLog = do
    h <- consoleHeight
    w <- consoleWidth
    let bw = 40
    let top = h - 4
    let left = w - bw + 1

    -- Draw box
    let space = [' ' | x <- [0..bw]]
    let line = ['=' | x <- [0..bw]]
    setCursor (left - 1) (top - 1)
    printIndentedLn (left - 2) $ "=" ++ line
    printIndentedLn (left - 2) $ "|" ++ space
    printIndentedLn (left - 2) $ "|" ++ space
    printIndentedLn (left - 2) $ "|" ++ space
    printIndentedLn (left - 2) $ "|" ++ space
    printIndentedLn (left - 2) $ "|" ++ space

    setCursor left top
    log <- readIORef gameLog
    let lines = take 5 log
    mapM_ (printIndentedLn left) lines

printIndentedLn :: Int -> String -> IO ()
printIndentedLn x s = do
    setCursorX x
    putStr s
    nextLine

draw :: [String] -> Position -> IO ()
draw m pos = do
    setCursor 0 0
    display_map m
    drawPlayer pos
    ms <- readIORef monsters
    drawMonsters ms
    drawHud pos
    setCursor 0 35
    hFlush stdout

keyPressed :: Char -> IO ()
keyPressed 'q' = writeIORef shouldQuit True
keyPressed _ = return()

main :: IO ()
main = do
    hideCursor
    cls
    let ms = [Monster "Troll" 20 (19, 9) 5]
    writeIORef monsters ms
    map <- loadMap "map.txt"
    writeIORef State.map map
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    playGame map
    showCursor

showTrace :: [Position] -> IO ()
showTrace [] = return ()
showTrace (p:ps) = do
    let (x, y) = p
    setCursor (x+1) (y+1)
    putStr "#"
    showTrace ps


playGame map = do
    handleAI

    p <- readIORef player
    let pos = Player.position p
    let coloredMap = colorMap $ computeVisible map pos
    draw coloredMap pos

    setCursor 3 3
    putStr "X"

    --let trace = dda map $ Ray (fromIntegral (fst pos), fromIntegral (snd pos)) (2.0, 2.0)
    --showTrace trace
    --setCursor 0 10
    --print trace

    a <- getHiddenChar
    cls
    setCursor 0 45
    if ord a == 0x1B then do
        handleControlSequence
    else if a == '\224' then do
        -- Windows is special ...
        a <- getHiddenChar
        winSpecialKeyPressed a
    else do
        keyPressed a
    sq <- readIORef shouldQuit
    if not sq then playGame map
    else return ()

handleAI :: IO ()
handleAI = do
    p <- readIORef player
    ms <- readIORef monsters
    map <- readIORef State.map
    let newMonsters = doMonsterStuff ms map p
    writeIORef monsters newMonsters
    return ()

doMonsterStuff :: [Monster] -> GameMap -> Player -> [Monster]
doMonsterStuff [] _ _ = []
doMonsterStuff (m:ms) map p = [handleMonster m map p] ++ (doMonsterStuff ms map p)

handleControlSequence :: IO ()
handleControlSequence = do
    a <- getHiddenChar
    _hcs1 a

_hcs1 :: Char -> IO ()
_hcs1 ('[') = do
    b <- getHiddenChar
    specialKeyPressed b

_hcs1 a = do
    putStrLn "Not proper sequence. Expected '[', got "
    print a
    return ()

winSpecialKeyPressed :: Char -> IO ()
winSpecialKeyPressed 'H' = specialKeyPressed 'A'
winSpecialKeyPressed 'P' = specialKeyPressed 'B'
winSpecialKeyPressed 'M' = specialKeyPressed 'C'
winSpecialKeyPressed 'K' = specialKeyPressed 'D'


specialKeyPressed :: Char -> IO ()
specialKeyPressed 'A' = moveRel (0, -1)
specialKeyPressed 'B' = moveRel (0, 1)
specialKeyPressed 'C' = moveRel (1, 0)
specialKeyPressed 'D' = moveRel (-1, 0)
