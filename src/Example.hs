{-# LANGUAGE TypeOperators, DataKinds, QuasiQuotes #-}
import Data.Record
import Data.IORef
import GHC.TypeLits

type Point 
 = '[ "x"       := Double
    , "y"       := Double
    , "z"       := Double 
    , "colour"  := (Int, Int, Int) ]

op :: Record P Point
op = 0 & 0 & 0 & (0,0,0) & end

main :: IO ()
main = do
    point <- runcomp newIORef op
    print =<< readIORef ([get|x|] point)
    print =<< readIORef ([get|y|] point)
    print =<< readIORef ([get|z|] point)
    print =<< readIORef ([get|colour|] point)

