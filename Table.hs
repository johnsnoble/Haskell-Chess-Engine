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
av = 373
bv = 5003
nullv = 255

powers :: Int -> Int -> UArray Int Int
powers a b = listArray (0,63) (f 64 [])
    where
        f :: Int -> [Int] -> [Int]
        f p [] = f (p-1) [mod a b]
        f p xs@(x:_)
            | p == 0 = xs
            | otherwise = f (p-1) ((mod (x*a) b):xs)

stPowers :: ST s (STUArray s Int Int)
stPowers = do
    arr <- newArray (0,63) 0
    writeArray arr 0 1
    forM_ [1..63] $ \i -> do
        prev <- readArray arr (i-1)
        writeArray arr i (mod (prev*373) 5003)
    return arr

{- hashState :: State -> (Int,Int,UArray Int Int) -> Int
hashState st (a,b,pws) = foldl1 f [hash i|i<-[0..2]]
    where
        f :: Int -> Int -> Int
        f x y = mod (x + y) b
        (_,_,bbs) = st
        bbPws = take 3 $ iterate (flip unsafeShiftL 1) 1
        hash :: Int -> Int
        hash n = foldl1 f [(pws!i)*(bbPws!!n)|i<-[0..63],testBit (bbs!(n+2)) i] -}

modAdd :: Int -> Int -> Int
modAdd x y = mod (x + y) bv

hashState :: STUArray s Int Int -> State -> ST s Int
hashState pws st = do
    -- must do everything imperatively to avoid taking out st monad and back in
    return $ foldl1 modAdd [hash i|i<-[0..2]]
    where
        (_,_,bbs) = st
        bbPws = take 3 $ iterate (flip unsafeShiftL 1) 1
        hash :: Int -> Int
        hash n = foldl1 modAdd [(pws!i)*(bbPws!!n)|i<-[0..63],testBit (bbs!(n+2)) i]

initStateTable :: Int -> ST s (STArray s Int (STUArray s Int Word64))
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

checkTable :: State -> ST s (STUArray s Int Int) -> ST s Bool
checkTable st table = undefined
--    = do
  --      let hashed = hashState st (av,bv,powers av bv)
        -- check if it exists in table

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

