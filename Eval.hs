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

bestMs :: Int -> State -> [(Int,Move)]
bestMs d st
    | d == 0 = []
    | otherwise = children
    where
        ms = getMoves st
        children = [(- alphabeta (move m st) (d-1) (-160,160),m)|m<-ms]

ab :: Int -> State -> (Int,Move)
ab d st
    = f ms (-160,(0,0))
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
        else f ms (-160,(0,0)) tt stt
    where
        f :: [Move] -> (Int,Move) -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s (Int,Move)
        f [] x _ _ = return x
        f (m:ms) x tt stt = do
            res <- stEval' (move m st) (d-1) (-160,-(fst x)) tt stt
            if (-res > (fst x))
                then f ms (-res,m) tt stt
                else f ms x tt stt

stEval' :: State -> Int -> Window -> STUArray s Int Int -> STArray s Int (STUArray s Int BBoard) -> ST s Int
stEval' st d (a,b) tt stt= do
    found <- see st tt stt
    if (found == Nothing)
        then return $ alphabeta st d (a,b)
        else return $ fromJust found

better :: (Int,a) -> (Int,a) -> (Int,a)
better p@(p',_) q@(q',_)
    | p' > q' = p
    | otherwise = q
