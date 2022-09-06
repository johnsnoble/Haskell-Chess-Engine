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

getArr :: Int -> ST s (STUArray s Int Int)
getArr size
    = do
        stArray <- newArray (0,size-1) nullEvalVal
        return stArray