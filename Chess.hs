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
type State = (Player,[[Cell]])

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
generateMove' (Taken (pl,t) pos) st@(pl',cs)
    | pl /= pl' = []
    | t == R = rockMove pos
    | t == B = bishopMove pos
    | t == Q = queenMove pos
    | t == K = kingMove pos
    | t == H = horseMove pos
    | t == P = pawnMove pos pl
    where
        genericMove :: Pos -> [Direction] -> Int -> State -> [Pos]
        genericMove pos dirs depth state
            = concat [genericMove' pos i depth state |i<-dirs]
            where
                genericMove' :: Pos -> Direction -> Int -> State -> [Pos]
                genericMove' p d n s
                    | n == 0 = []
                    | inBoard p' == False = []
                    | compare == 1 = []
                    | compare == -1 = [p']
                    | otherwise = p' : (genericMove' p' d (n-1) s)
                    where
                        p' = add p d
                        c = cellAt p' state
                        compare = sameTeam base c
                base = cellAt pos state

        rockMove :: Pos  -> [Pos]
        rockMove pos = genericMove pos norms 8 st
        bishopMove :: Pos ->  [Pos]
        bishopMove pos = genericMove pos diags 8 st
        queenMove :: Pos ->  [Pos]
        queenMove pos = genericMove pos (norms++diags) 8 st
        kingMove :: Pos ->  [Pos]
        kingMove pos = genericMove pos (norms++diags) 1 st
        horseMove :: Pos ->  [Pos]
        horseMove pos = genericMove pos horse 1 st
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

getMove :: Pos -> Player -> Direction -> [Pos]
getMove (x,y) pl (dx,dy)
    = undefined

move :: Pos -> Pos -> State -> State
move pos pos' state
    = (nextPlayer (fst state), cs')
    where
        Taken piece _ = cellAt pos state
        cs' = [[t (i,j)| j <- [0..7]] | i <- [0..7]]
        t :: Pos -> Cell
        t tPos
            | tPos == pos = Empty pos --Piece has moved
            | tPos == pos' = Taken piece pos' --Piece moved too
            | otherwise = cellAt tPos state --keep the same

inBoard :: Pos -> Bool
inBoard (x,y)
    = inRange x && inRange y
    where
        inRange :: Int -> Bool
        inRange t = (t >= 0) && (t < 8)

cellAt :: Pos -> State -> Cell
cellAt (x,y) (_,cs)
    = cs!!x!!y

printBoard :: State -> IO ()
printBoard (p,[])
    = do
        putStrLn (replicate 41 '-')
        putStrLn ((show p) ++ " to play")
printBoard (p,b:bs)
    = do
        printRow b
        printBoard (p,bs)
        where
            printRow :: [Cell] -> IO ()
            printRow row
                = do
                    putStrLn (replicate 41 '-')
                    putStrLn ("| " ++ pieces ++ " |")
                where
                    pieces = intercalate " | " (map show row)

newGame :: State
newGame
    = (L,board)
    where
        bL = [[Taken (D,R) (0,0), Taken (D,H) (0,1), Taken (D,B) (0,2), Taken (D,Q) (0,3), Taken (D,K) (0,4), Taken (D,B) (0,5), Taken (D,H) (0,6), Taken (D,R) (0,7)]]
        bP = [[Taken (D,P) (1,i)|i<-[0..7]]]
        gap = [[Empty (i,j)|j<-[0..7]]|i<-[2..5]]
        wP = [[Taken (L,P) (6,i)|i<-[0..7]]]
        wL = [[Taken (L,R) (7,0), Taken (L,H) (7,1), Taken (L,B) (7,2), Taken (L,Q) (7,3), Taken (L,K) (7,4), Taken (L,B) (7,5), Taken (L,H) (7,6), Taken (L,R) (7,7)]]
        board = bL ++ bP ++ gap ++ wP ++ wL

blank :: State
blank
    = (L,[[Empty (i,j)|j<-[0..7]]|i<-[0..7]])

queenGame :: State
queenGame = placeCell (placeCell (placeCell blank (Taken (L,Q) (3,4))) (Taken (D,R) (3,1))) (Taken (L,H) (7,4))

game :: State
game = placeCell (placeCell (placeCell blank (Taken (L,P) (6,2))) (Taken (D,H) (5,3))) (Taken (L,P) (5,1))

placeCell :: State -> Cell -> State
placeCell state@(pl,cs) cell@(Empty pos) = placePiece state cell pos
placeCell state@(pl,cs) cell@(Taken piece pos) = placePiece state cell pos

placePiece :: State -> Cell -> Pos -> State
placePiece state@(pl,cs) cell pos
    = (pl,cs')
    where
        cs' = [[t (i,j)| j <- [0..7]] | i <- [0..7]]
        t :: Pos -> Cell
        t pos'
            | pos' == pos = cell
            | otherwise = cellAt pos' state
