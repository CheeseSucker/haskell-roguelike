module Position where

type Position = (Int, Int)

translate :: Position -> Position -> Position
translate (x, y) (dx, dy) = (x + dx, y + dy)

directionTo (tx, ty) (sx, sy) = (fromIntegral $ round dx2, fromIntegral $ round dy2)
        where (dx, dy) = (tx - sx, ty - sy)
              dx2 = fromIntegral dx / vecLength (dx, dy)
              dy2 = fromIntegral dy / vecLength (dx, dy)

vecLength :: Position -> Float
vecLength (x, y) = sqrt $ fromIntegral (x*x + y*y)

vecSub :: Position -> Position -> Position
vecSub (a, b) (c, d) = (a - c, b - d)
 

p2f :: Position -> (Float, Float)
p2f (x, y) = (fromIntegral x, fromIntegral y)

f2p :: (Float, Float) -> Position
f2p (x, y) = (round x, round y)

