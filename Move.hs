module Move where

import Global
import Board
import Data.Word
import Data.Array.Unboxed
import Data.Bits

type Move = (Pos,Pos)

norms,diags,kqs,horses :: UArray Int Direction
norms = listArray (0,3) [-8,-1,1,8]
diags = listArray (0,3) [-9,-7,7,9]
horses = listArray (0,7) [-17,-15,-10,-6,6,10,15,17]
kqs = listArray (0,7) [-9,-8,-7,-1,1,7,8,9]

next :: Player -> Player
next L = D
next D = L

generateMoves :: State -> Pos -> [Pos]
generateMoves st@(pl,_,bb) pos
    | not $ inBoard pos = []
    | (isWh && (pl==L)) || (isBl && (pl==D)) = if b1
         then if b2
            then if b3
                then queenMove
                else rockMove
            else bishopMove
        else if b2
            then if b3
                then kingMove
                else horseMove
            else pawnMove
    | otherwise = []
    where
        isWh = testBit (bb!0) pos
        isBl = testBit (bb!1) pos
        b1 = testBit (bb!2) pos
        b2 = testBit (bb!3) pos
        b3 = testBit (bb!4) pos
        kingMove,queenMove,rockMove,bishopMove,horseMove,pawnMove :: [Pos]
        genericMove :: (Int,UArray Int Direction) -> Int -> [Pos]
        genericMove (len,dirs) depth
            = concat [genericMove' pos (dirs!i) depth|i<-[0..len-1]] -- TODO : try use map for this
            where
                genericMove' :: Pos -> Direction -> Int -> [Pos]
                genericMove' p d n
                    | n == 0 = []
                    | not $ connected p p' = []
                    | empty = p' : (genericMove' p' d (n-1))
                    | isWh' == isWh = []
                    | otherwise = [p']
                    where
                        p' = p + d
                        empty = not (isWh' || isBl')
                        isWh' = testBit (bb!0) p'
                        isBl' = testBit (bb!1) p'
        rockMove = genericMove (4,norms) 8
        bishopMove = genericMove (4,diags) 8
        queenMove = genericMove (8,kqs) 8
        horseMove = genericMove (8,horses) 1
        kingMove = [p|p<-ms,isSafe p]--f reach ms
            where
                ms = genericMove (8,kqs) 1
                reach = [pos+(kqs ! d)|d<-[0..7]]
        pawnMove
            | isWh = if and [(div pos 8) == 1,isEmpty (pos+16),isEmpty (pos+8)]
                then addAttack [pos+8,pos+16]
                else if isEmpty (pos+8) then addAttack [pos+8] else []
            | otherwise = if and [(div pos 8) == 6,isEmpty (pos-16),isEmpty (pos-8)]
                then addAttack [pos-8,pos-16]
                else if isEmpty (pos-8) then addAttack [pos-8] else []
            where
                isEmpty :: Pos -> Bool
                isEmpty p = (inBoard p) && (not ((testBit (bb!0) p) || (testBit (bb!1) p)))
                attack :: Pos -> [Pos] -> [Pos]
                attack p ps
                    | not $ connected pos p = ps
                    | isWh = if isWh' then ps
                        else if isBl' then addOpp:ps else ps
                    | isBl = if isBl' then ps
                        else if isWh' then addOpp:ps else ps
                    where
                        addOpp = p
                        isWh' = testBit (bb!0) p
                        isBl' = testBit (bb!1) p
                addAttack :: [Pos] -> [Pos]
                addAttack = (attack (pos+pl-1)).(attack (pos+pl+1))
                pl = if isWh then 8 else -8
        isExist :: Pos -> (Int,UArray Int Direction) -> Int -> [Cell] -> Bool
        isExist pos (l,dirs) depth pcs
            = or [isExist' pos (dirs ! i) depth|i<-[0..l-1]]
            where
                isExist' :: Pos -> Direction -> Int -> Bool
                isExist' p d n
                    | n == 0 = False
                    | not $ connected p p' = False
                    | empty = isExist' p' d (n-1)
                    | elem cell pcs = True
                    | otherwise = False
                    where
                        p' = p + d
                        cell = cellAt st p'
                        empty = not ((testBit cell 0) || (testBit cell 1))
        isSafe :: Pos -> Bool
        isSafe pos
            | isExist pos (8,kqs) 1 [colour .|. 24] = False
            | isExist pos (2,pawn) 1 [colour .|. 16] = False
            | isExist pos (4,diags) 8 [colour .|. 20, colour .|. 28] = False
            | isExist pos (4,norms) 8 [colour .|. 12, colour .|. 28] = False
            | isExist pos (8,horses) 1 [colour .|. 8] = False
            | otherwise = True
            where
                colour = if isWh then 0x2 else 0x1
                pawn = if isWh then listArray (0,2) [7,9] else listArray (0,2) [-7,-9]

{-
move : 11-9 {row} ,8-6 {col},5-3 {row'},2-0 {col'}
-}

move :: Move -> State -> State
move (a,b) st
    = (other pl, - sc + (cellScore pl b'), bb')
    where
        (pl,sc,bb') = (place 0 a (place a' b st))
        b' = cellAt st b
        a' = cellAt st a
