module Move where

import Global
import Board
import Data.Word
import Data.Array.Unboxed
import Data.Bits

type Move = Word16

norms,diags,kqs,horses :: UArray Int Direction
norms = listArray (0,3) [-8,-1,1,8]
diags = listArray (0,3) [-9,-7,7,9]
horses = listArray (0,7) [-17,-15,-10,-6,6,10,15,17]
kqs = listArray (0,7) [-9,-8,-7,-1,1,7,8,9]

next :: Player -> Player
next L = D
next D = L

generateMoves :: State -> Pos ->  BBoard
generateMoves st pos
    | testBit cell 0  = if testBit cell 1
                        then if testBit cell 2
                            then queenMove
                            else kingMove
                        else if testBit cell 2
                            then bishopMove
                            else pawnMove
    | otherwise = if testBit cell 1
                        then if testBit cell 2
                            then rockMove
                            else horseMove
                        else 0
    where
        cell = cellAt st pos
        genericMove :: (Int,UArray Int Direction) -> Int -> BBoard
        genericMove (len,dirs) depth
            = foldl (.|.) 0 [genericMove' pos (dirs!i) depth|i<-[0..len-1]]
            where
                genericMove' :: Pos -> Direction -> Int -> BBoard
                genericMove' p d n
                    | n == 0 = 0
                    | inBoard p' == False = 0
                    | empty = setBit (genericMove' p' d (n-1)) p'
                    | compare = 0
                    | otherwise = bit p'
                    where
                        p' = p + d
                        c = cellAt st p'
                        empty = c == 0
                        compare = (testBit cell 3) == (testBit c 3)

        kingMove,queenMove,rockMove,bishopMove,horseMove,pawnMove :: BBoard
        rockMove = genericMove (4,norms) 8
        bishopMove = genericMove (4,diags) 8
        queenMove = genericMove (8,kqs) 8
        horseMove = genericMove (8,horses) 1
        kingMove = undefined
        pawnMove = undefined

move :: Move -> State -> State
move m st = undefined

cellAt :: State -> Pos -> Cell
cellAt st pos
    = (f (at (b1 st)) 2) .|. ((f (at (b2 st)) 1) .|. (at (b3 st)))
    where
        toInt True = 1
        toInt False = 0
        f = unsafeShiftL
        at :: BBoard -> Word8
        at b = toInt $ testBit b pos
