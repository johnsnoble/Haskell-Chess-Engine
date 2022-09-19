module Eval where

import Board
import Move
import Global

import Data.Array.Base
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

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

better :: (Int,a) -> (Int,a) -> (Int,a)
better p@(p',_) q@(q',_)
    | p' > q' = p
    | otherwise = q
