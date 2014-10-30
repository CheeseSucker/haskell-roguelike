module Monster (
    Monster(..), handleMonster, translate
) where

import Position
import Player 
import Map

data Monster = Monster {
    _name :: String,
    _health :: Int,
    _position :: Position,
    _damage :: Int
} deriving (Show)

instance Actor Monster where
    name m = _name m
    health m = _health m
    position m = _position m
    canAttack m p = distanceBetween m p < 5

setPos :: Monster -> Position -> Monster
setPos m p = m { _position = p }

translateM :: Monster -> Position -> Monster
translateM m d = setPos m (translate d p)
        where p = position m


handleMonster :: Monster -> GameMap -> Player -> Monster
handleMonster m map p = moveTo m map (x + dx, y + dy)
            where (x, y) = position m
                  (dx, dy) = directionToPlayer p (x, y)


directionToPlayer :: Player -> Position -> Position
directionToPlayer p from = directionTo dest from
        where dest = position p

moveTo :: Monster -> GameMap -> Position -> Monster
moveTo m map dest | dest == pos  = m
                  | isWallAt map nextPos = m
                  | otherwise = moveTo (setPos m nextPos) map dest
        where   pos = position m
                nextPos = translate pos (directionTo dest pos)

