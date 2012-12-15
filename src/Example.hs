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

type User
  = '[ "nick"             := String
     , "real name"        := String
     , "gender"           := String
     , "country of birth" := String ]

main :: IO ()
main = do
    point <- runcomp newIORef op
    -- point :: RecordT IORef Point
        
    let pointx, pointy, pointz :: IORef Double
        pointc :: IORef (Int, Int, Int)
        pointx = [get|x|] point
        pointy = [get|y|] point
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
    -- 0.0 & 1.0 & 2.0 & (255,255,0) & end
    
    let greg :: Record User
        greg = "GREG" & "Sir Greg of Gerg" & "Male" & "Gregland" & end

        makeSpy :: Record User -> RecordT Maybe User
        makeSpy = [set|real name|] Nothing . box Just

    print greg
    -- "GREG" & "Sir Greg of Gerg" & "Male" & "Gregland" & end
   
    print (makeSpy greg)
    -- Just "GREG" & Nothing & Just "Male" & Just "Gregland" & end
    
    print (run (makeSpy greg))
    -- Nothing

