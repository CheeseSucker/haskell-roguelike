module Cell where

import Position

data Cell = Cell {
    position = Position,
    symbol = "",
    isBlocking = Bool,
    seen = Bool,
} derives (Show, Eq)


