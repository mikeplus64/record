{-# LANGUAGE DataKinds, TypeOperators, QuasiQuotes #-}
import Data.Record
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad.State
import Control.Arrow
import Data.Monoid

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

    let pointy3D = x &. y &. z &. nil :: RecordT Maybe Coord3D
        pointm3D = transform (Just . runIdentity) point3D

    let mf :: StateT (RecordT Maybe Coord3D) IO ()
        mf = do
            xm <- fields [key|x|]
            liftIO (print xm)
            [key|x|] =: Just 100
            [key|y|] ~: fmap (maybe 0 id xm +)
            [key|z|] =: Just 32
                
    print =<< runStateT mf pointy3D

    print . ($ (rempty :: RecordT Maybe Coord3D)) 
          $ write [key|x|] (Just (sqrt 2))
        >>> write [key|y|] (Just (sqrt 3))
        >>> write [key|z|] (Just (sqrt 4))

    print (combineWith (<|>) pointy3D pointm3D)

