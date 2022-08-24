module Bot where

import Chess

getMoves :: State -> [(Pos,Pos)]
getMoves state@(pl,_,cells)
    = moves cells'
    where
        cells' = concat cells
        moves :: [Cell] -> [(Pos,Pos)]
        moves [] = []
        moves (c@(Empty _):cs) = moves cs
        moves ((Taken _ pos):cs)
            = (map (f pos) (generateMove pos state)) ++ moves cs
        f :: Pos -> Pos -> (Pos,Pos)
        f pos pos' = (pos,pos')

eval :: State -> Int -> Int
eval state@(_,s,_) depth
    | depth == 0 = s
    | otherwise = - minimum children
    where
        ms = getMoves state
        children = [eval (move p q state) (depth-1)|(p,q)<-ms]

bestMove :: State -> Int -> (Int,(Pos,Pos))
bestMove state depth
    = foldl1 better children
    where
        ms = getMoves state
        children = [(eval (move p q state) (depth-1),m)|m@(p,q)<-ms]

better :: (Int,a) -> (Int,a) -> (Int,a)
better p@(p',_) q@(q',_)
    | p' > q' = p
    | otherwise = q

bestMs :: State -> Int -> (Int,[(Pos,Pos)])
bestMs state@(_,s,_) depth
    | depth == 0 = (s,[])
    | otherwise = foldl1 better children
    where
        ms = getMoves state
        children = [f m (bestMs (move p q state) (depth-1))|m@(p,q)<-ms]
        f :: (Pos,Pos) -> (Int,[(Pos,Pos)]) -> (Int,[(Pos,Pos)])
        f p (v,ps) = (v,p:ps)

computerMove :: State -> State
computerMove state
    = move start end state
    where
       (_,(start,end)) = bestMove state 3 --Must be an odd number

checkWinner :: State -> Bool
checkWinner (_,s,_) = (abs s) > 50

getWinner :: State -> Player
getWinner (pl,s,_)
    | s > 0 = pl
    | otherwise = nextPlayer pl
