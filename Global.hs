module Global where

import Data.Word

data Player = L | D
    deriving Eq

instance Show Player where
    show L = "W"
    show D = "B"

data Type = P | H | B | R | Q | K
    deriving Eq

instance Show Type where
    show P = "P"
    show H = "H"
    show B = "B"
    show R = "R"
    show Q = "Q"
    show K = "K"

type Piece = (Player,Type)

nullEvalVal :: Int
nullEvalVal = -256
