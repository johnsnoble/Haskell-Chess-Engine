module Eval where

import Board
import Move
import Global
import Table

import Data.Array.Base
import Data.Array.ST
import Data.Maybe
import Control.Monad
import Control.Monad.ST

type Window = (Int,Int)

nullRes :: (Int,Move)
nullRes = (-160,nullMove)

w0 :: Window
w0 = (-160,160)

getMoves :: State -> [Move]
getMoves state = concat ms
    where
        ms = [zip' i $ generateMoves state i|i<-[0..63]]
        zip' :: a -> [b] -> [(a,b)]
        zip' p ms = [(p,m)|m<-ms]

best :: Int -> State -> (Int,Move)
best depth st
    = foldl1 better children
    where
        ms = getMoves st
        children = [(-eval (depth-1) (move m st),m)|m<-ms]

eval :: Int -> State -> Int
eval d st@(_,sc,_)
    | d == 0 = sc
    | checkWinner st = sc
    | ms == [] = 0
    | otherwise = - minimum children
    where
        ms = getMoves st
        children = [eval (d-1) (move m st)|m<-ms]

bestMove :: Int -> State -> (Int,Move)
bestMove d st
    | d == 0 = nullRes
    | otherwise = foldl better nullRes children
    where
        ms = getMoves st
        children = [(- alphabeta (move m st) (d-1) w0,m)|m<-ms]

ab :: Int -> State -> (Int,Move)
ab d st
    = f ms nullRes
    where
        ms = getMoves st
        f :: [Move] -> (Int,Move) -> (Int,Move)
        f [] x = x
        f (m:ms) x
            | res > (fst x) = f ms (res,m)
            | otherwise = f ms x
            where
                res = - alphabeta (move m st) (d-1) (-160,- (fst x))

alphabeta :: State -> Int -> Window -> Int
alphabeta st@(_,sc,_) d (a,b)
    | d == 0 = sc
    | checkWinner st = sc
    | ms == [] = 0
    | otherwise = ab' ms a
    where
        ms = getMoves st
        ab' :: [Move] -> Int -> Int
        ab' [] x = x
        ab' (m:ms) x
            | res > b = res
            | otherwise = ab' ms x'
            where
                res = - alphabeta (move m st) (d-1) (-b,-x)
                x' = max x res

stEval :: Int -> State -> (Int,Move)
stEval d st = runST $ do
    let ms = getMoves st
    tt <- initTransTable bv
    stt <- initStateTable bv
    let l = length ms
    if l == 0
        then return (0,(0,0))
        else f ms nullRes tt stt
    where
        f :: [Move] -> (Int,Move) -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s (Int,Move)
        f [] x _ _ = return x
        f (m:ms) x tt stt = do
            res <- stEvalAB (move m st) (d-1) (-160,-(fst x)) tt stt
            if (-res > (fst x))
                then f ms (-res,m) tt stt
                else f ms x tt stt

stEvalAB :: State -> Int -> Window -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s Int
stEvalAB st@(_,sc,_) d (a,b) tt stt
    | d == 0 = return sc
    | checkWinner st = return sc
    | ms == [] = return 0
    | otherwise = do
        found <- see st tt stt
        if (found == Nothing)
            then ab' ms a tt stt
            else returnVal (fromJust found) tt stt
    where
        returnVal :: Int -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s Int
        returnVal x tt stt = write st x tt stt >> return x
        ms = getMoves st
        ab' :: [Move] -> Int -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s Int
        ab' [] x tt stt = returnVal x tt stt
        ab' (m:ms) x tt stt = do
            res <- stEvalAB (move m st) (d-1) (-b,-x) tt stt
            if (-res > b)
                then returnVal (-res) tt stt
                else ab' ms (max x (-res)) tt stt

better :: (Int,a) -> (Int,a) -> (Int,a)
better p@(p',_) q@(q',_)
    | p' > q' = p
    | otherwise = q
