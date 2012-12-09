{-# LANGUAGE TypeOperators, DataKinds, QuasiQuotes #-}
import Data.Record
import Control.Arrow

type Point
  = '[ "x" ::= Double 
     , "y" ::= Double 
     , "z" ::= Double ]

type Sphere
  = Point ++ '[ "radius" ::= Double ]

origin :: Record Point
origin = 0 ::: 0 ::: 0 ::: End

bigSphere :: Record Sphere
bigSphere = origin & 1737100 ::: End

otherSphere :: Record Sphere
otherSphere = ([key|y|] =: 340 >>> [key|x|] =: 1003) origin & 540 ::: End

main :: IO ()
main = do
    print origin
    print bigSphere
    print otherSphere

    putStrLn $ "x of the origin     = " ++ show (origin      ! [key|x|])
    putStrLn $ "radius of bigSphere = " ++ show (bigSphere   ! [key|radius|])
    putStrLn $ "x of otherSphere    = " ++ show (otherSphere ! [key|x|])

    putStrLn "What colour is otherSphere?"
    colour <- getLine
    putStrLn "Does it enjoy walks in the park?"
    walks  <- readLn

    let new = colour ::: walks ::: otherSphere :: Record ("colour" ::= String :+ "walks" ::= Bool :+ Sphere)
    putStrLn $ "To be honest, " ++ new ! [key|colour|] ++ " is a pretty crappy colour."
    putStrLn $ "Although I agree; I " ++ (if new ! [key|walks|] then "like" else "dislike") ++ " walks too."

