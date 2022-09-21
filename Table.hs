module Table where

import Data.Word
import Data.Bits
import Data.Array.Base
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Board
import Global
import Move

-- type StateTable = STArray s Int (STUArray s Int Word64)
-- type TransTable = STUArray s Int Int

av,bv,nullv :: Int
--av = 373
--bv = 5003
av = 2
bv = 5
nullv = 255

powers :: UArray Int Int
powers = listArray (0,63) (f 64 [])
    where
        f :: Int -> [Int] -> [Int]
        f p [] = f (p-1) [mod av bv]
        f p xs@(x:_)
            | p == 0 = xs
            | otherwise = f (p-1) ((mod (x*av) bv):xs)

modAdd :: Int -> Int -> Int
modAdd x y = mod (x + y) bv

hashState :: State -> Int
hashState st = foldl1 modAdd [hash i|i<-[0..2]]
    where
        pws = powers
        (_,_,bbs) = st
        bbPws = listArray (0,2) [1,2,4] :: UArray Int Int
        hash :: Int -> Int
        hash n = foldl1 modAdd [(pws!i)*(bbPws!n)|i<-[0..63],testBit (bbs!(n+2)) i]

initStateTable :: Int -> ST s (STArray s Int (STUArray s Int BBoard))
initStateTable size
    = do
        w <- newArray (0,size-1) 0
        b <- newArray (0,size-1) 0
        b1 <- newArray (0,size-1) 0
        b2 <- newArray (0,size-1) 0
        b3 <- newArray (0,size-1) 0
        arr <- newArray (0,4) w
        let bs = [w,b,b1,b2,b3]
        forM_ [0..4] $ \i -> do
            let board = bs !! i
            writeArray arr i board
        return arr

initTransTable :: Int -> ST s (STUArray s Int Int)
initTransTable size
    = do
        arr <- newArray (0,size-1) nullv
        return arr

writeTT :: State -> Int -> Int -> STUArray s Int Int -> ST s ()
writeTT st ind n table = writeArray table ind n

writeSTT :: State -> Int -> STArray s Int (STUArray s Int BBoard) -> ST s ()
writeSTT (_,_,bbs) ind table
    = do
        bbPtr <- readArray table ind
        forM_ [0..4] $ \i -> do
            writeArray bbPtr i (bbs!i)

write :: State -> Int -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s ()
write st n tt stt = do
    let hashed = hashState st
    writeTT st hashed n tt
    writeSTT st hashed stt

checkTable :: State -> STUArray s Int Int -> ST s (Maybe Int)
checkTable st table
    = do
        let hashed = hashState st
        val <- readArray table hashed
        if (val == nullv)
            then return Nothing
            else return (Just val)

testTT :: ST s (STUArray s Int Int)
testTT = do
    table <- initTransTable bv
    let ind = hashState t1
    writeTT t1 ind 6 table
    return table

testSTT :: ST s (STArray s Int (STUArray s Int BBoard))
testSTT = do
    table <- initStateTable bv
    let ind = hashState t1
    writeSTT t1 ind table
    return table

getSTT :: Int -> ST s (STUArray s Int BBoard)
getSTT ind = do
    table <- testSTT
    ret <- readArray table ind
    return ret

{-indexOf :: State -> Int
indexOf = undefined

checkTable :: StateTable -> State -> ST s Bool
checkTable table state
    = do
        let ind = indexOf state
        w <- readArray state 0
        b <- readArray state 1
        b1 <- readArray state 2
        b2 <- readArray state 3
        b3 <- readArray state 4-}
emptys :: Int -> ST s (STUArray s Int Word64)
emptys size
    = do
        arr <- newArray (0,size-1) 0
        return arr

