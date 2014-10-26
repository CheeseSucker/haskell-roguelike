module TerminalStuff where

import System.IO
import System.Environment

consoleWidth :: IO Int
consoleWidth = do 
    e <- getEnv("COLUMNS")
    return $ read e

consoleHeight :: IO Int
consoleHeight = do
    e <- getEnv("LINES")
    return $ read e

-- ANSI: 0 = Clear to end, 1 = Clear to beginning, 2 = Clear all
cls :: IO ()
cls = do
    runAnsi "2J"  -- Clear screen
    setCursor 0 0 -- Set cursor to (0, 0) for compatibility with DOS

setCursor :: Int -> Int -> IO ()
setCursor x y = runAnsi $ (show y) ++ ";" ++ (show x) ++ "H"

setCursorX :: Int -> IO ()
setCursorX x = runAnsi $ (show x) ++ "G" 

runAnsi :: String -> IO ()
runAnsi cmd = putStr $ "\x1b[" ++ cmd

clearLine :: IO ()
clearLine = runAnsi "K"

nextLine :: IO ()
nextLine = runAnsi "1B"

prevLine :: IO ()
prevLine = runAnsi "1A"

hideCursor :: IO ()
hideCursor = runAnsi "?25l"

showCursor :: IO ()
showCursor = runAnsi "?25h"

-- Colors
_createColor :: Int -> String
_createColor c = "\x1b[" ++ (show c) ++ "m"

clrReset = _createColor 0
clrBold = _createColor 1
clrNoBold = _createColor 22
clrBlink = _createColor 5
clrNoBlink = _createColor 25

clrBlack = _createColor 30
clrRed = _createColor 31
clrGreen = _createColor 32
clrYellow = _createColor 33
clrBlue = _createColor 34
clrMagenta = _createColor 35
clrCyan = _createColor 36
clrWhite = _createColor 37
clrNormalColor = _createColor 39

clrBgBlack = _createColor 40
clrBgRed = _createColor 41
clrBgGreen = _createColor 42
clrBgYellow = _createColor 43
clrBgBlue = _createColor 44
clrBgMagenta = _createColor 45
clrBgCyan = _createColor 46
clrBgWhite = _createColor 47
clrBgNormalColor = _createColor 49


{-
Position the Cursor:
  \033[<L>;<C>H
     Or
  \033[<L>;<C>f
  puts the cursor at line L and column C.
- Move the cursor up N lines:
  \033[<N>A
- Move the cursor down N lines:
  \033[<N>B
- Move the cursor forward N columns:
  \033[<N>C
- Move the cursor backward N columns:
  \033[<N>D

- Clear the screen, move to (0,0):
  \033[2J
- Erase to end of line:
  \033[K
-} 
