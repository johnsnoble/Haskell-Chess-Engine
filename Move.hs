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

generateMoves :: State -> Pos -> [(Int,Pos)]
generateMoves st pos
    | not $ inBoard pos = []
    | not (isWh || isBl) = []
    | p3 = if p2
        then if p1
            then queenMove
            else kingMove
        else if p1
            then bishopMove
            else pawnMove
    | otherwise = if p1
        then rockMove
        else horseMove
    where
        isWh = testBit (white st) pos
        isBl = testBit (black st) pos
        p3 = testBit (b3 st) pos
        p2 = testBit (b2 st) pos
        p1 = testBit (b1 st) pos
        kingMove,queenMove,rockMove,bishopMove,horseMove,pawnMove :: [(Int,Pos)]
        genericMove :: (Int,UArray Int Direction) -> Int -> [(Int,Pos)]
        genericMove (len,dirs) depth
            = concat [genericMove' pos (dirs!i) depth|i<-[0..len-1]] -- TODO : try use map for this
            where
                genericMove' :: Pos -> Direction -> Int -> [(Int,Pos)]
                genericMove' p d n
                    | n == 0 = []
                    | not $ connected p p' = []
                    | empty = (0,p') : (genericMove' p' d (n-1))
                    | isWh' == isWh = []
                    | otherwise = [(-score,p')]
                    where
                        p' = p + d
                        empty = not (isWh' || isBl')
                        isWh' = testBit (white st) p'
                        isBl' = testBit (black st) p'
                        score = cellScore isWh (cellAt st p')
        rockMove = genericMove (4,norms) 8
        bishopMove = genericMove (4,diags) 8
        queenMove = genericMove (8,kqs) 8
        horseMove = genericMove (8,horses) 1
        kingMove = [m|m@(_,p)<-ms,isSafe p]--f reach ms
            where
                ms = genericMove (8,kqs) 1
                reach = [pos+(kqs ! d)|d<-[0..7]]
        pawnMove
            | isWh = if and [(div pos 8) == 1,isEmpty (pos+16),isEmpty (pos+8)]
                then addAttack [(0,pos+8),(0,pos+16)]
                else if isEmpty (pos+8) then addAttack [(0,pos+8)] else []
            | otherwise = if and [(div pos 8) == 6,isEmpty (pos-16),isEmpty (pos-8)]
                then addAttack [(0,pos-8),(0,pos-16)]
                else if isEmpty (pos-8) then addAttack [(0,pos-8)] else []
            where
                isEmpty :: Pos -> Bool
                isEmpty p = (inBoard p) && (not ((testBit (white st) p) || (testBit (black st) p)))
                attack :: Pos -> [(Int,Pos)] -> [(Int,Pos)]
                attack p ps
                    | not $ connected pos p = ps
                    | isWh = if isWh' then ps
                        else if isBl' then addOpp:ps else ps
                    | isBl = if isBl' then ps
                        else if isWh' then addOpp:ps else ps
                    where
                        addOpp = (-sc,p)
                        sc = cellScore isWh (cellAt st p)
                        isWh' = testBit (white st) p
                        isBl' = testBit (black st) p
                addAttack :: [(Int,Pos)] -> [(Int,Pos)]
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
                    | cell == 0 = isExist' p' d (n-1)
                    | elem cell pcs = True
                    | otherwise = False
                    where
                        p' = p + d
                        cell = cellAt st p'
        isSafe :: Pos -> Bool
        isSafe pos
            | isExist pos (8,kqs) 1 [colour .|. 0x3] = False
            | isExist pos (2,pawn) 1 [colour .|. 0x1] = False
            | isExist pos (4,diags) 8 [colour .|. 0x5, colour .|. 0x7] = False
            | isExist pos (4,norms) 8 [colour .|. 0x6, colour .|. 0x7] = False
            | isExist pos (8,horses) 1 [colour .|. 0x2] = False
            | otherwise = True
            where
                colour = if isWh then 0x0 else 0x8
                pawn = if isWh then listArray (0,2) [7,9] else listArray (0,2) [-7,-9]

isExist :: State -> Pos -> (Int,UArray Int Direction) -> Int -> [Cell] -> Bool
isExist st pos (l,dirs) depth pcs
    = or [isExist' pos (dirs ! i) depth|i<-[0..l-1]]
    where
        isExist' :: Pos -> Direction -> Int -> Bool
        isExist' p d n
            | n == 0 = False
            | not $ connected p p' = False
            | cell == 0 = isExist' p' d (n-1)
            | elem cell pcs = True
            | otherwise = False
            where
                p' = p + d
                cell = cellAt st p'
isSafe :: State -> Bool -> Pos -> Bool
isSafe st isWh pos
    | isExist st pos (8,kqs) 1 [colour .|. 0x3] = False
    | isExist st pos (2,pawn) 1 [colour .|. 0x1] = False
    | isExist st pos (4,diags) 8 [colour .|. 0x5, colour .|. 0x7] = False
    | isExist st pos (4,norms) 8 [colour .|. 0x6, colour .|. 0x7] = False
    | isExist st pos (8,horses) 1 [colour .|. 0x2] = False
    | otherwise = True
    where
        colour = if isWh then 0x0 else 0x8
        pawn = if isWh then listArray (0,2) [7,9] else listArray (0,2) [-7,-9]

{-
move : 11-9 {row} ,8-6 {col},5-3 {row'},2-0 {col'}
-}
move :: Move -> State -> State
move m st = undefined

