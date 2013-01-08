{-# LANGUAGE OverloadedStrings
           , DeriveFunctor
           , DeriveTraversable
           , UndecidableInstances
           , DeriveFoldable
           , DataKinds
           , PolyKinds
           , TypeFamilies
           , TemplateHaskell
           , GADTs
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , FunctionalDependencies
           , TypeOperators #-}

module Data.Record.ZipcordT where
import Data.Record
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Data.Monoid
import Data.Foldable    as F
import Data.Traversable as T
import Control.Comonad
import Control.Applicative
import Control.Lens
import Control.Arrow

data ZipcordT w xs ys k a = ZipcordT
    { _zleft  :: RecordT w xs
    , _zfocus :: w a
    , _zright :: RecordT w ys
    } deriving (Eq, Ord, Show, Functor)

makeLenses ''ZipcordT

pee :: (Applicative f, Comonad w, Comonad m) => w (a -> b) -> m a -> f b
pee f x = pure (extract f (extract x))

instance ( EmptyRecord w xs
         , EmptyRecord w ys
         , Applicative w
         , Comonad w)
        => Applicative (ZipcordT w xs ys k) where

    pure a = ZipcordT rempty (pure a) rempty

    ZipcordT _ f _ <*> ZipcordT xs a ys
        = ZipcordT xs (f `pee` a) ys

instance Comonad w => Foldable (ZipcordT w xs ys k) where
    foldMap f (ZipcordT _ a _) = f (extract a)

instance (Applicative w, Comonad w) => Traversable (ZipcordT w xs ys k) where
    sequenceA (ZipcordT xs a ys) 
        = ZipcordT <$> pure xs 
                   <*> fmap pure (extract a)
                   <*> pure ys

instance (Applicative w, Comonad w) => Comonad (ZipcordT w xs ys k) where
    extract    (ZipcordT _  a _ ) = extract a
    duplicate  (ZipcordT xs a ys) = ZipcordT xs (pure (ZipcordT xs a ys)) ys
    extend f z@(ZipcordT xs _ ys) = ZipcordT xs (pure (f z)) ys

instance (Applicative w, Comonad w) => ComonadApply (ZipcordT w xs ys k) where
    ZipcordT _ f _ <@> ZipcordT xs a ys = ZipcordT xs (f `pee` a) ys

push :: ZipcordT w (k := x ': xs) ys               k' a'
     -> ZipcordT w xs             (k' := a' ': ys) k  x
push (ZipcordT (C x xs) y ys) = ZipcordT xs x (C y ys)

pull :: ZipcordT w xs               (k := y ': ys) k' a'
     -> ZipcordT w (k' := a' ': xs) ys             k  y
pull (ZipcordT xs x (C y ys)) = ZipcordT (C x xs) y ys

shift :: Int -> Q Exp
shift 0 = [| id |]
shift i = Prelude.foldr1 
    (\l r -> [| (.) |] `appE` l `appE` r) 
    (replicate (abs i) (if i > 0 then [| push |] else [| pull |]))

