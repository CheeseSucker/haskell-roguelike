module Vision.Bresenheim (bresenheim) where

import Position

-- Bresenheim Line Algorithm
footprint = bresenheim

bresenheim :: Position -> Position -> [Position]
bresenheim start end | sx > dx = reverse $ bresenheim end start
                      | abs deltaX < abs deltaY = [(x, y) | (y, x) <- bresenheim (sy, sx) (dy, dx)]
                      | otherwise = _bresenheim end dErr e (0 * signum dErr) start
    where dErr = deltaY / (deltaX + 1)
          (sx, sy) = start
          (dx, dy) = end
          deltaX = fromIntegral $ abs (dx - sx)
          deltaY = fromIntegral (dy - sy)
          e = (abs deltaY) / (abs deltaY + 1)


_bresenheim :: Position -> Double -> Double -> Double -> Position -> [Position]
_bresenheim (dx,_) _ _ _ (x,_) | x > dx = []
_bresenheim end dErr e err pos = [pos] ++ ( 
    if abs totErr < e 
    then bres totErr (x+1, y)
    else bres (totErr - e * signum totErr) (x+1, y + round (signum totErr)))
    where totErr = dErr + err
          (x, y) = pos
          bres = _bresenheim end dErr e

