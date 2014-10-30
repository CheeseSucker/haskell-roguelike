module Map where

import Position
import Data.List
import Data.Maybe
import TerminalStuff
import System.IO

type GameMap = [String]

outOfBounds :: GameMap -> Position -> Bool
outOfBounds _ (x, y) | x < 0 || y < 0 = True
outOfBounds m (_, y) | y >= length m = True
outOfBounds m (x, y) = x >= length (m !! y)

mapElm :: GameMap -> Position -> Char
mapElm m (x, y) | outOfBounds m (x, y) = '='
mapElm m (x, y) = (m !! y) !! x

isWallAt :: GameMap -> Position -> Bool 
isWallAt m pos = outOfBounds m pos || (isWall $ mapElm m pos)

isWall :: Char -> Bool
isWall c | c == '=' = True
isWall c | c == '|' = True
isWall _ = False

loadMap :: FilePath -> IO GameMap
loadMap fileName = do
    print "Loading file"
    file <- openFile fileName ReadMode
    contents <- _loadMap file 
    hClose file
    print "done"
    return contents

_loadMap :: Handle -> IO GameMap
_loadMap h = do
    eof <- hIsEOF h 
    if eof then return []
    else do
        line <- hGetLine h
        rest <- _loadMap h
        return $ [line] ++ rest

-- Coloring the map --
colorMap :: GameMap -> [String]
colorMap [] = []
colorMap (x:xs) = [(colorStr x)] ++ colorMap xs

colorStr :: String -> String
colorStr c | c == "" = clrReset -- Reset colors 
colorStr (c:cs) = (colorChr c) ++ colorStr cs

colorChr :: Char -> String 
colorChr c 
        | c == '=' = clrYellow ++ "=" -- 2593
        | c == '@' = clrGreen ++ "@" -- 26b2 
        | c == '|' = clrYellow ++ "|" -- 2593
        | c == 'X' = clrRed ++ "X" -- 🐭
        | otherwise = clrWhite ++ [c]


