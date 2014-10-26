module Vision.Unrealistic where

import Map
import Position

getElements :: GameMap -> Position -> Position -> [Char]
getElements m start end = map (mapElm m) $ footprint start end

footprint :: Position -> Position -> [Position]
footprint start end | start == end = []
footprint start end = [start] ++ footprint next end
    where next = translate start $ directionTo end start
