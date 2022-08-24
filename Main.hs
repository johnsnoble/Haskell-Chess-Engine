module Main where

import Chess
import Bot
import Data.Char

{-- main :: IO ()
main
    = do
        winner <- game newGame
        putStrLn ("\n" ++ (show winner) ++ " Won!") --}

main :: IO ()
main
    = do
        winner <- game1 newGame
        putStrLn ("\n" ++ (show winner) ++ " Won!") 

game1 :: State -> IO Player
game1 state
    = do
        if (checkWinner state)
            then return (getWinner state)
            else do
                printBoard state
                state' <- userMove state
                game2 state'

game2 :: State -> IO Player
game2 state
    = do
        if (checkWinner state)
            then return (getWinner state)
            else do
                printBoard state
                let state' = computerMove state
                game1 state'

game' :: State -> IO Player
game' state@(pl,score,_)
    = do
        if (score > 50)
            then (return pl)
            else
                if (score < -50)
                    then return (nextPlayer pl)
                else
                    do
                        putStrLn ""
                        printBoard state
                        state' <- userMove state
                        game' state'

userMove :: State -> IO State
userMove state
    = do
        start <- userInput "Pick Piece to move: "
        end <- userInput "Input Destination: "
        if (isValid start end state)
            then return (move start end state)
            else userMove state

userInput :: [Char] -> IO (Int,Int)
userInput str
    = do
        putStrLn str
        y <- userInt
        x <- userInt
        return (x-1,y-1)

userInt :: IO Int
userInt
    = do
        c <- getChar
        let n = digitToInt c
        if (n > 0 && n <= 8)
            then return n
            else userInt
