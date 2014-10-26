module Player where

import Position
import Map

class Actor a where
    name :: a -> String
    position :: a -> Position
    canAttack :: a -> a -> Bool
    health :: a -> Int
    isDead :: a -> Bool
    isDead a = health a <= 0


data Player = Player {  
    pName :: String,
    pPosition :: Position,
    pHealth :: Int,
    pDamage :: Int
} deriving (Show)

instance Actor Player where
    name p = pName p
    position p = pPosition p
    health p = pHealth p
    canAttack p m = distanceBetween p m < 2

distanceBetween :: Actor a => a -> a -> Float
distanceBetween a b = vecLength (pa `vecSub` pb)
    where pa = position a
          pb = position b
