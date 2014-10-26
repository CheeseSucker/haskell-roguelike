module Vision.DDA (dda_cell, dda) where

import Ray
import Map
import Position

-- footprint :: Position -> Position -> [Position]
dda_cell :: GameMap -> Position -> Position -> Maybe Ray
dda_cell map start end = dda_cell' map end rays   
     where rays = [Ray (fromIntegral sx + x, fromIntegral sy + y) (fromIntegral dx, fromIntegral dy) | x <- [0,0.5,1], y <- [0,0.5,1]]
           (sx, sy) = start
           (dx, dy) = end

dda_cell' :: GameMap -> Position -> [Ray] -> Maybe Ray
dda_cell' m end [] = Nothing
dda_cell' m end (r:rays) = if dda_ray m end r then Just r else dda_cell' m end rays

data DDAState = DDAState {
    _map :: GameMap,
    _ray :: Ray,
    _dDistX :: Float,
    _dDistY :: Float,
    _stepX :: Int,
    _stepY :: Int
}

dda_ray :: GameMap -> Position -> Ray -> Bool
dda_ray m end ray = if last (dda m ray) == end then True else False


dda :: GameMap -> Ray -> [Position]
dda m ray = _dda (DDAState m ray dDistX dDistY stepX stepY) sideDistX sideDistY x y 
    where dDistX = sqrt(1.0 + (rayDirY ** 2 / rayDirX ** 2))
          dDistY = sqrt(1.0 + (rayDirX ** 2 / rayDirY ** 2))
          (rayDirX, rayDirY) = direction ray
          stepX = if rayDirX < 0 then -1 else 1
          stepY = if rayDirY < 0 then -1 else 1
          sideDistX = (if rayDirX < 0 then (rayPosX - cx) else (cx + 1.0 - rayPosX)) * dDistX
          sideDistY = (if rayDirY < 0 then (rayPosY - cy) else (cy + 1.0 - rayPosY)) * dDistY
          (x, y) = getCell $ Ray.start ray
          (cx, cy) = (fromIntegral x, fromIntegral y)
          (rayPosX, rayPosY) = Ray.start ray

_dda :: DDAState -> Float -> Float -> Int -> Int -> [Position]
_dda s _ _ cx cy | isWallAt (_map s) (cx, cy) = [(cx, cy)]
                 | _distance (start r) (cx, cy) >= rayLength r = [getCell $ Ray.end r]
    where r = _ray s
_dda s sdx sdy cx cy = [(cx, cy)] ++ 
         if sdx < sdy then 
            _dda s (sdx + _dDistX s) sdy (cx + _stepX s) cy
         else
            _dda s sdx (sdy + _dDistY s) cx (cy + _stepY s)

getCell :: (Float, Float) -> Position
getCell (x, y) = (round x, round y)

_distance :: (Float, Float) -> Position -> Float
_distance start (cx, cy) = rayLength $ Ray start (fromIntegral cx, fromIntegral cy)


