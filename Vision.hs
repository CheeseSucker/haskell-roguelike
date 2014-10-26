module Vision where

import Data.List
import Data.Maybe
import Map
import Position
import Vision.Unrealistic

-- Brute force visibility algorithm
computeVisible :: GameMap -> Position -> GameMap
computeVisible map pos = doVisibilityBF map pos 0

doVisibilityBF :: GameMap -> Position -> Int -> GameMap
doVisibilityBF map pos row | row >= length map = []
                           | otherwise = [checkRow map pos row 0] 
                                ++ doVisibilityBF map pos (row+1)

checkRow :: GameMap -> Position -> Int -> Int -> String
checkRow map pos y x | x >= length (map !! y) = "" 
                     | not visible = " " ++ checkRow map pos y (x+1)
                     | otherwise = [if c == ' ' then '.' else c] ++ checkRow map pos y (x+1)
                where visible = isVisible map pos (x, y)
                      c = mapElm map (x, y)

isVisible :: GameMap -> Position -> Position -> Bool
isVisible map start end = isNothing $ find isWall elms
    where elms = getElements map start end

