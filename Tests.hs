{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TypeOperators, ScopedTypeVariables #-}
import Control.Monad.Identity
import Test.QuickCheck
import Data.Record
import Data.IORef

type Point a
  = '[ "x" := a
     , "y" := a
     , "z" := a ]

main :: IO ()
main = do
    quickCheck $ \ x y z -> 
        let point  :: Record (Point Int)
            point  = x & y & z & nil 
        
            mpoint :: RecordT (Maybe :.: Identity) (Point Int)
            mpoint = box Just point

            mpoint' :: RecordT (Maybe :.: Identity) (Point Int)
            mpoint' = [set|x|] (compose (const Nothing)) mpoint

        in and
            [ mpoint                    == Just (Identity x) & Just (Identity y) & Just (Identity z) & nil :: RecordT (Maybe :.: Identity) (Point Int)
            , [get|x|] mpoint           == Just (Identity x)
            , [get|y|] mpoint           == Just (Identity y)
            , [get|z|] mpoint           == Just (Identity z)
            , mpoint'                   == Nothing & Just (Identity y) & Just (Identity z) & nil :: RecordT (Maybe :.: Identity) (Point Int)
            , run mpoint'               == Nothing
            ]
