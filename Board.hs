module Board where

import Data.Word
import Data.Array.Unboxed
import Global hiding (State)

type BBoard = Word64
type Cell = Word8

rows,cols :: UArray Int BBoard

rows = listArray (0,7) [
    0xFF,
    0xFF00,
    0xFF0000,
    0xFF000000,
    0xFF00000000,
    0xFF0000000000,
    0xFF000000000000,
    0xFF00000000000000 ]

cols = listArray (0,7) [
    0x0101010101010101,
    0x0202020202020202,
    0x0404040404040404,
    0x0808080808080808,
    0x1010101010101010,
    0x2020202020202020,
    0x4040404040404040,
    0x8080808080808080 ]

data State = State {
    white, black :: BBoard,
    b1, b2, b3 :: BBoard,
    {-        b1 b2 b3
        Pawn   0  0  1
        King   0  1  1
        Horse  0  1  0
        Bishop 1  0  1
        Rock   1  1  0
        Queen  1  1  1
    -}
    score :: Int,
    player :: Player
}

emptyState :: State
emptyState = State {white = 0, black = 0, b1 = 0, b2 = 0, b3 = 0, score = -1, player = L}

{-newGame :: state
newGame = State {
    white = 0x000000000000FFFF,
    black = 0xFFFF000000000000,
    b1 = 0xAD000000000000AD,
    b2 = 0xDB000000000000DB,
    b3 = 0x3CFF00000000FF3C,
    score = 0,
    player = L
}-}
