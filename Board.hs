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
type State = (Player,Int,UArray Int BBoard)

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

    {-        b1 b2 b3
        Pawn   0  0  1
        King   0  1  1
        Horse  0  1  0
        Bishop 1  0  1
        Rock   1  1  0
        Queen  1  1  1
    -}

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

other :: Player -> Player
other L = D
other D = L

score :: State -> Int
score (_,x,_) = x

setScore :: Int -> State -> State
setScore n (pl,sc,bb) = (pl,n,bb)

player :: State -> Player
player (x,_,_) = x

checkWinner :: State -> Bool
checkWinner st = abs (score st) > 50

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
    putStrLn $ (show (player st)) ++ " to play"
    putStrLn $ (show (player st)) ++ ": " ++ (show (score st))
    where
        println :: Int -> IO ()
        println n = do
            putStrLn ("| " ++ line ++ " |")
            where
                cs = reverse [showCell (cellAt st i)|i<-[8*n..8*(n+1)-1]]
                line = intercalate " | " cs

showCell :: Cell -> [Char]
showCell cell
    | testBit cell 0 = 'W':(show piece)
    | testBit cell 1 = 'B':(show piece)
    | otherwise = "  "
    where
    piece = withType cell (P,H,B,R,Q,K)

withType :: Cell -> (a,a,a,a,a,a) -> a
withType cell (p,h,b,r,q,k) = piece
    where
        code = unsafeShiftR cell 2
        piece = if testBit code 0
            then if testBit code 1
                then if testBit code 2
                    then q
                    else r
                else b
            else if testBit code 1
                then if testBit code 2
                    then k
                    else h
                else p

cellAt' :: State -> Pos -> Cell
cellAt' (_,_,st) pos
    = foldl (.|.) 0 [at (st ! 0),f (at (st ! 1)) 1,f (at (st ! 2)) 2,f (at (st ! 3)) 3,f (at (st ! 4)) 4]
    where
        toInt True = 1
        toInt False = 0
        f = unsafeShiftL
        at :: BBoard -> Word8
        at b = toInt $ testBit b pos

cellAt :: State -> Pos -> Cell
cellAt (_,_,st) pos
    = foldl (.) id [flip setBit i|i<-[0..4],testBit (st!i) pos] 0

cellScore :: Player -> Cell -> Int
cellScore pl cell
    | not (isBl' || isWh') = 0
    | otherwise = opp*val
    where
        opp = if isWh == isWh' then 1 else -1
        val = withType cell (1,3,3,5,9,99)
        isWh = pl == L
        isWh' = testBit cell 0
        isBl' = testBit cell 1

place :: Cell -> Pos -> State -> State
place cell pos (pl,sc,bb)
    = (pl,sc,bb')
    where
        bb' = listArray (0,4) [f i|i<-[0..4]]
        f :: Int -> BBoard
        f x
            | testBit cell x = setBit (bb!x) pos
            | otherwise = clearBit (bb!x) pos

evalScore :: State -> Int
evalScore st = foldl1 (+) [cellScore (player st) (cellAt st i)|i<-[0..63]]

create :: [(Cell,Pos)] -> State
create x = setScore (evalScore st) st
    where
        fs = [place c p|(c,p)<-x]
        f = foldl1 (.) fs 
        st = f emptyState

emptyState :: State
emptyState = (L,0,listArray (0,4) [0,0,0,0,0])

newGame :: State
newGame = (L,0, listArray (0,4) [
    0xFFFF, --White
    0xFFFF000000000000, --Black
    0xB5000000000000B5, --B,R,Q
    0xDB000000000000DB, --H,K,R,Q
    0x3CFF00000000FF3C ])--P,K,B,Q

t1 :: State
t1 = create $ zip [26,18,18,18,18,29,17,25,30,17,17,17,13,21,13] [57,55,50,40,33,28,27,26,16,15,14,13,7,5,2]
