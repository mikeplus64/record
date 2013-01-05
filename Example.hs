{-# LANGUAGE DataKinds, TypeOperators, QuasiQuotes #-}
import Data.Record
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad.State

type Coord2D
    = '[ "x" := Double
       , "y" := Double ]

type Coord3D
    = Coord2D
   ++ '[ "z" := Double ]

writeM :: (Monad m, Update r k a) => Key k -> a -> RecordT m r -> RecordT m r
writeM k x = write k (return x)

main :: IO ()
main = do
    let point2D = 0 & 0.5 & nil           :: Record Coord2D
        point3D = union point2D (3 & nil) :: Record Coord3D

    print $ access [key|x|] point2D
    print $ access [key|y|] point2D
    print point3D

    x <- readMaybe `fmap` getLine
    y <- readMaybe `fmap` getLine
    z <- readMaybe `fmap` getLine

    let pointy3D = x &- y &- z &- nil :: RecordT Maybe Coord3D
        pointm3D = transform (Just . runIdentity) point3D

    print (combineWith (<|>) pointy3D pointm3D)

