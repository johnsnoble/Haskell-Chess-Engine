module Global where

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

data Cell = Empty Pos | Taken Piece Pos
    deriving Eq

instance Show Cell where
    show (Empty _) = "  "
    show (Taken (a,b) _) = (show a) ++ (show b)

type Pos = (Int,Int)
type Direction = (Int,Int)
type State = (Player,Int,[[Cell]])
type Move = (Pos,Pos)

nullEvalVal :: Int
nullEvalVal = -256