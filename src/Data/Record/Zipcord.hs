{-# LANGUAGE OverloadedStrings
           , DeriveFunctor
           , DataKinds
           , PolyKinds
           , TemplateHaskell
           , GADTs
           , FlexibleInstances
           , FlexibleContexts
           , TypeOperators #-}

module Data.Record.Zipcord where
import Data.Record
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Data.Monoid
import Data.Foldable    as F
import Data.Traversable as T
import Control.Comonad
import Control.Applicative

data ZipcordT w xs ys k a = ZipcordT
    { _zleft  :: RecordT w xs
    , _zfocus :: w a
    , _zright :: RecordT w ys
    } deriving (Eq, Ord, Show, Functor)

{-# INLINE pee #-}
pee :: (Applicative f, Comonad w, Comonad m) => w (a -> b) -> m a -> f b
pee f x = pure (extract f (extract x))

instance ( Monoid (w a)
         , Monoid (RecordT w xs)
         , Monoid (RecordT w ys)) 
        => Monoid (ZipcordT w xs ys k a) where

    {-# INLINE mempty #-}
    mempty = ZipcordT mempty mempty mempty

    {-# INLINE mappend #-}
    mappend (ZipcordT xs' a' ys') (ZipcordT xs  a  ys )
        = ZipcordT (xs' `mappend` xs) 
                   (a'  `mappend` a)
                   (ys' `mappend` ys)

instance ( EmptyRecord w xs
         , EmptyRecord w ys
         , Applicative w
         , Comonad w)
        => Applicative (ZipcordT w xs ys k) where

    {-# INLINE pure #-}
    pure a = ZipcordT rempty (pure a) rempty

    {-# INLINE (<*>) #-}
    ZipcordT _ f _ <*> ZipcordT xs a ys
        = ZipcordT xs (f `pee` a) ys

instance Comonad w => Foldable (ZipcordT w xs ys k) where
    {-# INLINE foldMap #-}
    foldMap f (ZipcordT _ a _) = f (extract a)

instance (Applicative w, Comonad w) => Traversable (ZipcordT w xs ys k) where
    {-# INLINE sequenceA #-}
    sequenceA (ZipcordT xs a ys) 
        = ZipcordT <$> pure xs 
                   <*> fmap pure (extract a)
                   <*> pure ys

instance (Applicative w, Comonad w) => Comonad (ZipcordT w xs ys k) where
    {-# INLINE extract #-}
    {-# INLINE duplicate #-}
    {-# INLINE extend #-}
    extract    (ZipcordT _  a _ ) = extract a
    duplicate  (ZipcordT xs a ys) = ZipcordT xs (pure (ZipcordT xs a ys)) ys
    extend f z@(ZipcordT xs _ ys) = ZipcordT xs (pure (f z)) ys

instance (Applicative w, Comonad w) => ComonadApply (ZipcordT w xs ys k) where
    {-# INLINE (<@>) #-}
    ZipcordT _ f _ <@> ZipcordT xs a ys = ZipcordT xs (f `pee` a) ys

-- | "Push" a zipcord -- focus the leftmost element, and cons the current
-- focus to the right record.
{-# INLINE push #-}
push :: ZipcordT w (k := x ': xs) ys               k' a'
     -> ZipcordT w xs             (k' := a' ': ys) k  x
push (ZipcordT (C x xs) y ys) = ZipcordT xs x (C y ys)

-- | "Pull" a zipcord -- focus the rightmost element, and cons the current
-- focus to the left record.
{-# INLINE pull #-}
pull :: ZipcordT w xs               (k := y ': ys) k' a'
     -> ZipcordT w (k' := a' ': xs) ys             k  y
pull (ZipcordT xs x (C y ys)) = ZipcordT (C x xs) y ys

-- | Push or pull a zipcord *n* times.
-- > *n*  < 0 = *n* calls to 'pull'
-- > *n*  > 0 = *n* calls to 'push'
-- > *n* == 0 = id
shift :: Int -> Q Exp
shift 0 = [| id |]
shift i = Prelude.foldr1 
    (\l r -> [| (.) |] `appE` l `appE` r) 
    (replicate (abs i) (if i > 0 then [| push |] else [| pull |]))

