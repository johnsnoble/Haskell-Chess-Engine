module Bot where

import Chess

getMoves :: State -> [Move]
getMoves state@(pl,_,cells)
    = moves cells'
    where
        cells' = concat cells
        moves :: [Cell] -> [Move]
        moves [] = []
        moves (c@(Empty _):cs) = moves cs
        moves ((Taken _ pos):cs)
            = (map (f pos) (generateMove pos state)) ++ moves cs
        f :: Pos -> Pos -> Move
        f pos pos' = (pos,pos')

eval :: State -> Int -> Int
eval state@(_,s,_) depth
    | depth == 0 = s
    | checkWinner state = s
    | ms == [] = 0
    | otherwise = - minimum children
    where
        ms = getMoves state
        children = [eval (move m state) (depth-1)|m<-ms]

bestMove :: State -> Int -> (Int,Move)
bestMove state depth
    = foldl1 better children
    where
        ms = getMoves state
        children = [(-eval (move m state) (depth-1),m)|m<-ms]

better :: (Int,a) -> (Int,a) -> (Int,a)
better p@(p',_) q@(q',_)
    | p' > q' = p
    | otherwise = q

bestMs :: State -> Int -> (Int,[Move])
bestMs state@(_,s,_) depth
    | depth == 0 = (s,[])
    | otherwise = foldl1 better children
    where
        ms = getMoves state
        children = [f m (bestMs (move m state) (depth-1))|m<-ms]
        f :: Move -> (Int,[Move]) -> (Int,[Move])
        f p (v,ps) = (v,p:ps)

alphabeta :: State -> Int -> (Int,Move)
alphabeta state depth
    = f ms (-160,((0,0),(0,0)))
    where
        ms = getMoves state
        f :: [Move] -> (Int,Move) -> (Int,Move)
        f [] x = x
        f (m:ms) (x,m')
            | res >= x = f ms (res,m)
            | otherwise = f ms (x,m')
            where
                res = -ab (move m state) (depth-1) (-160,-x)

type Window = (Int,Int)
ab :: State -> Int -> Window -> Int
ab state@(_,s,_) depth (a,b)
    | depth == 0 = s
    | checkWinner state = s
    | ms == [] = 0
    | otherwise = ab' ms a
    where
        ms = getMoves state
        ab' :: [Move] -> Int -> Int
        ab' [] x = x
        ab' (m:ms) x
            | res > b = res
            | otherwise = ab' ms x'
            where
                res = - ab (move m state) (depth-1) (-b,-x)
                x' = max x res

computerMove :: State -> State
computerMove state
    = move m state
    where
       (_,m) = alphabeta state 3 --Must be an odd number

getStatus :: State -> Int
getStatus (_,s,_)
    | s > 50 = 1
    | s < 50 = -1
    | otherwise = 0

checkWinner :: State -> Bool
checkWinner (_,s,_) = (abs s) > 50

getWinner :: State -> Player
getWinner (pl,s,_)
    | s > 0 = pl
    | otherwise = nextPlayer pl
