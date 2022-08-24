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
    | otherwise = - minimum children
    where
        ms = getMoves state
        children = [eval (move m state) (depth-1)|m<-ms]

bestMove :: State -> Int -> (Int,Move)
bestMove state depth
    = foldl1 better children
    where
        ms = getMoves state
        children = [(eval (move m state) (depth-1),m)|m<-ms]

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

computerMove :: State -> State
computerMove state
    = move m state
    where
       (_,m) = bestMove state 3 --Must be an odd number

checkWinner :: State -> Bool
checkWinner (_,s,_) = (abs s) > 50

getWinner :: State -> Player
getWinner (pl,s,_)
    | s > 0 = pl
    | otherwise = nextPlayer pl
