module Chess where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

data Player = L | D
    deriving Eq

instance Show Player where
    show L = "W"
    show D = "B"

data Type = P | H | B | R | Q | K
    deriving Eq

instance Show Type where
    show P = "P"
    show H = "H"
    show B = "B"
    show R = "R"
    show Q = "Q"
    show K = "K"

type Piece = (Player,Type)

data Cell = Empty Pos | Taken Piece Pos
    deriving Eq

instance Show Cell where
    show (Empty _) = "  "
    show (Taken (a,b) _) = (show a) ++ (show b)

type Pos = (Int,Int)
type Direction = (Int,Int)
type State = (Player,Int,[[Cell]])
type Move = (Pos,Pos)

nextPlayer :: Player -> Player
nextPlayer L = D
nextPlayer D = L

add :: (Int,Int) -> (Int,Int) -> (Int,Int)
add (x,y) (x',y') = (x+x',y+y')

norms :: [Direction]
norms = [(0,1),(1,0),(0,-1),(-1,0)]
diags :: [Direction]
diags = [(1,1),(-1,1),(1,-1),(-1,-1)]
horse :: [Direction]
horse = [(1,2),(2,1),(-1,2),(-2,1),(1,-2),(2,-1),(-1,-2),(-2,-1)]

sameTeam :: Cell -> Cell -> Int
sameTeam (Taken (pl,_) _) (Taken (pl',_) _)
    | pl == pl' = 1
    | otherwise = -1
sameTeam _ _ = 0

generateMove :: Pos -> State -> [Pos]
generateMove pos state = generateMove' (cellAt pos state) state

generateMove' :: Cell -> State -> [Pos]
generateMove' (Empty pos) _ = []
generateMove' (Taken (pl,t) pos) st@(pl',_,cs)
    | pl /= pl' = []
    | t == R = rockMove pos
    | t == B = bishopMove pos
    | t == Q = queenMove pos
    | t == K = kingMove pos
    | t == H = horseMove pos
    | t == P = pawnMove pos pl
    where
        genericMove :: Pos -> [Direction] -> Int -> [Pos]
        genericMove pos dirs depth
            = concat [genericMove' pos i depth|i<-dirs]
            where
                genericMove' :: Pos -> Direction -> Int -> [Pos]
                genericMove' p d n
                    | n == 0 = []
                    | inBoard p' == False = []
                    | compare == 1 = []
                    | compare == -1 = [p']
                    | otherwise = p' : (genericMove' p' d (n-1))
                    where
                        p' = add p d
                        c = cellAt p' st
                        compare = sameTeam base c
                base = cellAt pos st
        isExist :: Pos -> [Direction] -> Int -> [Piece] -> Bool
        isExist pos dirs depth pcs
            = or [isExist' pos i depth|i<-dirs]
            where
                isExist' :: Pos -> Direction -> Int -> Bool
                isExist' p d n
                    | n == 0 = False
                    | inBoard p' == False = False
                    | cellAt p' st == (Empty p') = isExist' p' d (n-1)
                    | elem pc pcs = True
                    | otherwise = False
                    where
                        p' = add p d
                        (Taken pc _) = cellAt p' st
        isSafe :: Player -> Pos -> Bool
        isSafe pl pos
            | isExist pos (diags++norms) 1 [(pl',K)] = False
            | (pl == L) && isExist pos [(-1,-1),(-1,1)] 1 [(D,P)] = False
            | (pl == D) && isExist pos [(1,-1),(1,1)] 1 [(L,P)] = False
            | isExist pos diags 8 [(pl',B),(pl',Q)] = False
            | isExist pos norms 8 [(pl',R),(pl',Q)] = False
            | isExist pos horse 1 [(pl',H)] = False
            | otherwise = True
            where
                pl' = nextPlayer pl
        rockMove :: Pos  -> [Pos]
        rockMove pos = genericMove pos norms 8
        bishopMove :: Pos ->  [Pos]
        bishopMove pos = genericMove pos diags 8
        queenMove :: Pos ->  [Pos]
        queenMove pos = genericMove pos (norms++diags) 8
        kingMove :: Pos ->  [Pos]
        kingMove pos
            = [m|m<-ms,isSafe pl m]
            where
                ms = genericMove pos (norms++diags) 1
                (Taken (pl,_) _) = cellAt pos st
        horseMove :: Pos ->  [Pos]
        horseMove pos = genericMove pos horse 1
        pawnMove :: Pos -> Player -> [Pos]
        pawnMove pos@(x,y) L
            | x == 6 && emptyAt (4,y) && emptyAt(5,y) = f [(4,y),(5,y)]
            | x > 0 && emptyAt (x-1,y) = f [(x-1,y)]
            | otherwise = [] -- implement promotion
            where
                f :: [Pos] -> [Pos]
                f = (attack (x-1,y-1)).(attack (x-1,y+1))
        pawnMove pos@(x,y) D
            | x == 1 && emptyAt (2,y) && emptyAt (3,y) = f[(2,y),(3,y)]
            | x < 7 && emptyAt (x+1,y) = f [(x+1,y)]
            | otherwise = []
            where
                f :: [Pos] -> [Pos]
                f = (attack (x+1,y-1)).(attack (x+1,y+1))
        attack :: Pos -> [Pos] -> [Pos]
        attack pos'@(x',y') ps
            | inBoard (x',y') /= True = ps
            | sameTeam (cellAt pos' st) (cellAt pos st) == -1 = pos':ps
            | otherwise = ps
        emptyAt :: Pos -> Bool
        emptyAt pos = (cellAt pos st == Empty pos)

isStalemate :: State -> Bool
isStalemate state@(pl,_,cells)
    = isStalemate' cells'
    where
        cells' = concat cells
        isStalemate' :: [Cell] -> Bool
        isStalemate' [] = True
        isStalemate' ((Empty _):cs) = isStalemate' cs
        isStalemate' (c@(Taken _ pos):cs)
            | generateMove pos state == [] = isStalemate' cs
            | otherwise = False

getScore :: Cell -> Int
getScore (Empty _) = 0
getScore (Taken (_,P) _) = 1
getScore (Taken (_,H) _) = 3
getScore (Taken (_,B) _) = 3
getScore (Taken (_,R) _) = 5
getScore (Taken (_,Q) _) = 9
getScore (Taken (_,K) _) = 99

move' :: Pos -> Pos -> State -> State
move' pos pos' state@(pl,score,_)
    = (nextPlayer pl,-score',cs')
    where
        Taken piece _ = cellAt pos state
        score' = score + getScore (cellAt pos' state)
        cs' = [[t (i,j)| j <- [0..7]] | i <- [0..7]]
        t :: Pos -> Cell
        t tPos
            | tPos == pos = Empty pos --Piece has moved
            | tPos == pos' = Taken piece pos' --Piece moved too
            | otherwise = cellAt tPos state --keep the same

move :: Move -> State -> State
move (pos,pos') state@(pl,score,_)
    = (nextPlayer pl,-score',cs')
    where
        Taken piece _ = cellAt pos state
        score' = score + getScore (cellAt pos' state)
        cs' = [[t (i,j)| j <- [0..7]] | i <- [0..7]]
        t :: Pos -> Cell
        t tPos
            | tPos == pos = Empty pos --Piece has moved
            | tPos == pos' = Taken piece pos' --Piece moved too
            | otherwise = cellAt tPos state --keep the same

isValid :: Move -> State -> Bool
isValid (pos,pos') state = elem pos' (generateMove pos state)

skipTurn :: State -> State
skipTurn (pl,s,cs) = (nextPlayer pl,-s,cs)

inBoard :: Pos -> Bool
inBoard (x,y)
    = inRange x && inRange y
    where
        inRange :: Int -> Bool
        inRange t = (t >= 0) && (t < 8)

cellAt :: Pos -> State -> Cell
cellAt (x,y) (_,_,cs)
    = cs!!x!!y

getHeuristic :: State -> Int
getHeuristic (_,score,_) = score

printBoard :: State -> IO ()
printBoard (pl,s,cs)
    = print cs'
    where
        cs' = zip [1..8] cs
        print :: [(Int,[Cell])] -> IO ()
        print [] 
            = do
                putStrLn ("  " ++ (replicate 41 '-'))
                putStrLn ("    " ++(intercalate "    " (map show [1..8])))
                putStrLn ((show pl) ++ " to play")
                putStrLn ((show pl) ++ ": " ++ (show s))
        print ((n,b):bs)
            = do
                printRow n b
                print bs
        printRow :: Int -> [Cell] -> IO ()
        printRow n row
            = do
                putStrLn ("  " ++ (replicate 41 '-'))
                putStrLn ((show n) ++ " | " ++ pieces ++ " |")
            where
                pieces = intercalate " | " (map show row)

newGame :: State
newGame
    = (L,0,board)
    where
        bL = [[Taken (D,R) (0,0), Taken (D,H) (0,1), Taken (D,B) (0,2), Taken (D,Q) (0,3), Taken (D,K) (0,4), Taken (D,B) (0,5), Taken (D,H) (0,6), Taken (D,R) (0,7)]]
        bP = [[Taken (D,P) (1,i)|i<-[0..7]]]
        gap = [[Empty (i,j)|j<-[0..7]]|i<-[2..5]]
        wP = [[Taken (L,P) (6,i)|i<-[0..7]]]
        wL = [[Taken (L,R) (7,0), Taken (L,H) (7,1), Taken (L,B) (7,2), Taken (L,Q) (7,3), Taken (L,K) (7,4), Taken (L,B) (7,5), Taken (L,H) (7,6), Taken (L,R) (7,7)]]
        board = bL ++ bP ++ gap ++ wP ++ wL

blank :: State
blank
    = (L,0,[[Empty (i,j)|j<-[0..7]]|i<-[0..7]])

queenGame :: State
queenGame = createGame [Taken (L,Q) (3,4),Taken (D,R) (3,1),Taken (L,H) (7,4)] blank

pawnGame :: State
pawnGame = createGame [Taken (L,P) (6,2),Taken (D,H) (5,3),Taken (L,P) (5,1)] blank

kingGame :: State
kingGame = createGame [Taken (L,K) (7,7),Taken (D,Q) (5,6)] blank

e1 :: State
e1 = createGame [Taken (L,K) (1,0),Taken (L,P) (1,1),Taken (L,P) (1,2),Taken (L,H) (0,3),Taken (D,P) (1,4),Taken (D,K) (2,3),Taken (L,R) (3,4),Taken (L,P) (4,3),Taken (L,R) (5,6),Taken (L,B) (6,7)] blank

e1a :: State
e1a = createGame [Taken (L,K) (1,0),Taken (L,P) (1,1),Taken (D,K) (1,2),Taken (L,H) (0,3),Taken (D,P) (1,4),Taken (L,R) (3,4),Taken (L,P) (4,3),Taken (L,R) (1,6),Taken (L,B) (6,7)] blank

e2 :: State
e2 = skipTurn (createGame [Taken (D,K) (0,6),Taken (D,P) (1,0),Taken (D,P) (1,5),Taken (D,P) (1,6),Taken (D,P) (1,7),Taken (L,K) (3,6),Taken (L,Q) (4,3),Taken (L,P) (4,4),Taken (D,Q) (5,7),Taken (L,P) (6,0),Taken (L,P) (6,1),Taken (L,P) (6,2),Taken (L,R) (7,0),Taken (L,B) (7,2),Taken (L,R) (7,5)] blank)

e2a = skipTurn (createGame [Taken (D,K) (0,6),Taken (D,P) (1,0),Taken (D,P) (1,5),Taken (D,P) (1,6),Taken (D,P) (2,7),Taken (L,K) (4,5),Taken (L,Q) (4,3),Taken (L,P) (4,4),Taken (D,Q) (5,7),Taken (L,P) (6,0),Taken (L,P) (6,1),Taken (L,P) (6,2),Taken (L,R) (7,0),Taken (L,B) (7,2),Taken (L,R) (7,5)] blank)

e2b = createGame [Taken (D,K) (0,6),Taken (D,P) (1,0),Taken (D,P) (1,5),Taken (D,P) (3,6),Taken (D,P) (2,7),Taken (L,K) (4,5),Taken (L,Q) (4,3),Taken (L,P) (4,4),Taken (D,Q) (5,7),Taken (L,P) (6,0),Taken (L,P) (6,1),Taken (L,P) (6,2),Taken (L,R) (7,0),Taken (L,B) (7,2),Taken (L,R) (7,5)] blank

createGame :: [Cell] -> State -> State
createGame [] state = state
createGame (c:cs) state
    = placeCell c (createGame cs state)

placeCell :: Cell -> State -> State
placeCell cell@(Empty pos) state = placePiece state cell pos
placeCell cell@(Taken piece pos) state = placePiece state cell pos

placePiece :: State -> Cell -> Pos -> State
placePiece state@(pl,score,cs) cell pos
    = (pl,score',cs')
    where
        cs' = [[t (i,j)| j <- [0..7]] | i <- [0..7]]
        score' = score + (sameTeam (Taken (pl,P) (8,8)) cell)*(getScore cell)
        t :: Pos -> Cell
        t pos'
            | pos' == pos = cell
            | otherwise = cellAt pos' state
