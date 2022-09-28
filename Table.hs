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
hashState st = foldl modAdd 0 [hash i|i<-[0..2]]
    where
        pws = powers
        (_,_,bbs) = st
        bbPws = listArray (0,2) [1,2,4] :: UArray Int Int
        hash :: Int -> Int
        hash n = foldl modAdd 0 [(pws!i)*(bbPws!n)|i<-[0..63],testBit (bbs!(n+2)) i]

initStateTable :: Int -> ST s (STArray s Int (STUArray s Int BBoard))
initStateTable size = do
    table <- newArray_ (0,size-1)
    forM_ [0..size-1] $ \i -> (newArray (0,4) 0 >>= \st -> writeArray table i st)
    return table

initTransTable :: Int -> ST s (STUArray s Int Int)
initTransTable size = newArray (0,size-1) nullv >>= \arr -> return arr

writeTT :: Int -> Int -> STUArray s Int Int -> ST s ()
writeTT ind n table = writeArray table ind n

writeSTT :: State -> Int -> STArray s Int (STUArray s Int BBoard) -> ST s ()
writeSTT (_,_,bbs) ind table
    = do
        bbPtr <- readArray table ind
        forM_ [0..4] $ \i -> writeArray bbPtr i (bbs!i)

write :: State -> Int -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s ()
write st n tt stt = do
    let hashed = hashState st
    writeTT hashed n tt
    writeSTT st hashed stt

compareState :: State -> Int -> STArray s Int (STUArray s Int BBoard) -> ST s Bool
compareState (_,_,bbs) ind table = do
    stbbs <- readArray table ind
    w <- readArray stbbs 0
    b <- readArray stbbs 1
    b1 <- readArray stbbs 2
    b2 <- readArray stbbs 3
    b3 <- readArray stbbs 4
    let bbs' = [w,b,b1,b2,b3]
    return $ compare bbs bbs'
    where
        compare :: UArray Int BBoard -> [BBoard] -> Bool
        compare a b = compare' 0 b
            where
                compare' :: Int -> [BBoard] -> Bool
                compare' _ [] = True
                compare' n (b:bs)
                    | (a!n) /= b = False
                    | otherwise = compare' (n+1) bs

see :: State -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s (Maybe Int)
see st tt stt = do
    let hashed = hashState st
    val <- readArray tt hashed
    if val == nullv
        then return Nothing
        else do
            found <- compareState st hashed stt
            if found
                then return (Just val)
                else return Nothing

add1 :: ST s (STUArray s Int Int,STArray s Int (STUArray s Int BBoard))
add1 = do
    tt <- initTransTable bv
    stt <- initStateTable bv
    write t1 3 tt stt
    return (tt,stt)

add2 :: STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s (STUArray s Int Int,STArray s Int (STUArray s Int BBoard))
add2 tt stt = write emptyState 6 tt stt >> return (tt,stt)

add3 :: ST s (Maybe Int)
add3 = do
    (tt,stt) <- add1
    (tt',stt') <- add2 tt stt
    see t1 tt stt
