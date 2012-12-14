{-# LANGUAGE TypeOperators, DataKinds, QuasiQuotes #-}
import Data.Record
import Data.IORef

type Point 
 = '[ "x"       := Double
    , "y"       := Double
    , "z"       := Double 
    , "colour"  := (Int, Int, Int) ]

op :: Record P Point
op = 0 & 0 & 0 & (0,0,0) & end

main :: IO ()
main = do
    let p = box (compose newIORef) op
    print "yes"


