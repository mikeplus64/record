{-# LANGUAGE TypeOperators, DataKinds, QuasiQuotes #-}
import Data.Record
import Data.IORef
import GHC.TypeLits

type Point 
 = '[ "x"       := Double
    , "y"       := Double
    , "z"       := Double 
    , "colour"  := (Int, Int, Int) ]

op :: Record Point
op = 0 & 0 & 0 & (0,0,0) & end

main :: IO ()
main = do
    point <- runcomp newIORef op
 -- point :: RecordT IORef Point
        
    let pointx = [get|x|] point
        pointy = [get|x|] point
        pointz = [get|z|] point
        pointc = [get|colour|] point

    writeIORef pointx 0
    writeIORef pointy 1
    writeIORef pointz 2
    writeIORef pointc (255, 255, 0)

    -- "freeze" the record
    frozenPoint <- runtrans readIORef point
 -- frozenPoint :: Record Point
    print frozenPoint

