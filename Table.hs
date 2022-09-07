module Table where

import Data.Word
import Data.Array.Base
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Board
import Global hiding (State)

hashState :: State -> Int
hashState = undefined

data MyType = MyType {
    x :: Int,
    y :: Bool
}

emptyType :: MyType
emptyType = MyType { x = 2, y = True }

initStateTable :: Int -> ST s (STArray s Int (STUArray s Int Word64))
initStateTable size
    = do
        w <- newArray (0,size-1) 0
        b <- newArray (0,size-1) 0
        b1 <- newArray (0,size-1) 0
        b2 <- newArray (0,size-1) 0
        b3 <- newArray (0,size-1) 0
        arr <- newArray (0,size-1) w
        let bs = [w,b,b1,b2,b3]
        forM_ [0..4] $ \i -> do
            let board = bs !! i
            writeArray arr i board
        return arr

initTT :: Int -> ST s (STUArray s Int Int)
initTT size
    = do
        arr <- newArray (0,size-1) nullEvalVal
        return arr
