module Ray where

import Position

data Ray = Ray {
    start :: (Float, Float), 
    end :: (Float, Float)
}

direction :: Ray -> (Float, Float)
direction ray = (dx - sx, dy - sy) `vecDiv`  rayLength ray
    where (sx, sy) = start ray
          (dx, dy) = end ray

rayLength :: Ray -> Float
rayLength ray = sqrt (x ** 2 + y ** 2)
    where (sx, sy) = start ray
          (dx, dy) = end ray
          (x, y) = (dx - sx, dy - sy)

vecDiv :: (Float, Float) -> Float -> (Float, Float)
vecDiv (x, y) l = (x / l, y / l)

