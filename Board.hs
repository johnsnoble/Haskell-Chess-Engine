module Board where

import Data.Word
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Control.Monad
import Global

type BBoard = Word64
type Cell = Word8
type Pos = Int
type Direction = Int

-- Cell colour {1}, type {3} 

rows :: UArray Int BBoard

rows = listArray (0,7) [
    0xFF,
    0xFF00,
    0xFF0000,
    0xFF000000,
    0xFF00000000,
    0xFF0000000000,
    0xFF000000000000,
    0xFF00000000000000 ]

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

inBoard :: Pos -> Bool
inBoard pos = (pos < 64) && (pos >= 0)

connected :: Pos -> Pos -> Bool
connected p1 p2
    | not $ inBoard p2 = False
    | col1 == 7 = not (col2 == 0)
    | col1 == 0 = not (col2 == 7)
    | otherwise = True
    where
        col1 = mod p1 8
        col2 = mod p2 8

toType :: (Bool,Bool,Bool) -> Type
toType (b1,b2,b3)
    | and [b1,b2,b3] = Q
    | b1 && b2 = R
    | b1 && b3 = B
    | b2 && b3 = K
    | b2 = H
    | otherwise = P

printBB :: BBoard -> IO ()
printBB b = do
    forM_ [7,6..0] $ \i -> do
        println i
    where
        println :: Int -> IO ()
        println n
            = do
                putStrLn bs
                where
                    l = unsafeShiftR ((rows ! n) .&. b) (8*n)
                    bs = ((map f).reverse) [testBit l i|i<-[0..7]]
        f :: Bool -> Char
        f False = '0'
        f True = '1'

printSt :: State -> IO ()
printSt st = do
    forM_ [7,6..0] $ \i -> do
        putStrLn $ replicate 41 '-'
        println i
    putStrLn $ replicate 41 '-'
    where
        println :: Int -> IO ()
        println n = do
            putStrLn ("| " ++ line ++ " |")
            where
                cs = reverse [showCell (cellAt st i)|i<-[8*n..8*(n+1)-1]]
                line = intercalate " | " cs

showCell :: Cell -> [Char]
showCell cell
    | cell == 0 = "  "
    | piece == 1 = [pl,'P']
    | piece == 2 = [pl,'H']
    | piece == 3 = [pl,'K']
    | piece == 5 = [pl,'B']
    | piece == 6 = [pl,'R']
    | piece == 7 = [pl,'Q']
    | otherwise = "  "
    where
        pl = if testBit cell 3
            then 'W'
            else 'B'
        piece = cell .&. 0x7

cellAt :: State -> Pos -> Cell
cellAt st pos
    =  (f (at (white st)) 3) .|. ((f (at (b1 st)) 2) .|. ((f (at (b2 st)) 1) .|. (at (b3 st))))
    where
        toInt True = 1
        toInt False = 0
        f = unsafeShiftL
        at :: BBoard -> Word8
        at b = toInt $ testBit b pos

cellScore :: Bool -> Cell -> Int
cellScore pl cell
    = opp*(pieceVal piece)
    where
        piece = cell .&. 0x7
        opp = if pl == testBit cell 3 then 1 else -1
        pieceVal :: Cell -> Int
        pieceVal n
            | n == 1 = 1
            | n == 2 = 3
            | n == 3 = 99
            | n == 5 = 3
            | n == 6 = 5
            | n == 7 = 9
            | otherwise = 0

place :: Cell -> Pos -> State -> State
place cell pos st
    | cell == 0 = pack cleared 
    | otherwise = pack (bl:[f i|i<-[0..3]])
    where
        ss = [black st,white st,b1 st,b2 st,b3 st]
        cleared = [clearBit i pos|i<-ss]
        f :: Int -> BBoard
        f n
            | testBit cell (3-n) = setBit c pos
            | otherwise = c
            where
                c = cleared !! (n+1)
        bl = if testBit cell 3 then head cleared else setBit (head cleared) pos
        pl = testBit cell 3
        pack :: [BBoard] -> State
        pack [bl,wh,b1,b2,b3] = State {white = wh, black = bl, b1 = b1, b2 = b2, b3 = b3,score = (score st) + cellScore pl cell, player = player st}

create :: [(Cell,Pos)] -> State
create x = f emptyState
    where
        fs = [place c p|(c,p)<-x]
        f = foldl1 (.) fs 

emptyState :: State
emptyState = State {white = 0, black = 0, b1 = 0, b2 = 0, b3 = 0, score = -1, player = L}

newGame :: State
newGame = State {
    white = 0xFFFF,
    black = 0xFFFF000000000000,
    b1 = 0xB5000000000000B5, --B,R,Q
    b2 = 0xDB000000000000DB, --H,K,R,Q
    b3 = 0x3CFF00000000FF3C, --P,K,B,Q
    score = 0,
    player = L
}

t1 :: State
t1 = create $ zip [3,1,1,1,1,15,9,11,7,9,9,9,14,13,14] [57,55,50,40,33,28,27,26,16,15,14,13,7,5,2]
