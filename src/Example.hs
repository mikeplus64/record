{-# LANGUAGE TypeOperators, DataKinds, QuasiQuotes #-}
import Data.Record

type Point
  = '[ "x" ::= Double 
     , "y" ::= Double 
     , "z" ::= Double ]

type Sphere
  = Point ++ '[ "radius" ::= Double ]

origin :: Record Point
origin = 0 ::: 0 ::: 0 ::: End

bigSphere :: Record Sphere
bigSphere = origin +++ 1737100 ::: End

otherSphere :: Record Sphere
otherSphere = ([upd|y|] 340 . [upd|x|] 1003) origin +++ 540 ::: End

main :: IO ()
main = do
    print origin
    print bigSphere
    print otherSphere

    putStrLn $ "x of the origin     = " ++ show ([get|x|] origin)
    putStrLn $ "radius of bigSphere = " ++ show ([get|radius|] bigSphere)
    putStrLn $ "x of otherSphere    = " ++ show ([get|x|] otherSphere)

    putStrLn "What colour is otherSphere?"
    colour <- getLine
    putStrLn "Does it enjoy walks in the park?"
    walks  <- readLn

    let new = colour ::: walks ::: otherSphere :: Record ("colour" ::= String :+ "walks" ::= Bool :+ Sphere)
    putStrLn $ "To be honest, " ++ [get|colour|] new ++ " is a pretty crappy colour."
    putStrLn $ "Although I agree; I " ++ (if [get|walks|] new then "like" else "dislike") ++ " walks too."

