{-# LANGUAGE GADTs
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , DataKinds
           , TypeOperators
           , PolyKinds
           , EmptyDataDecls
           , Rank2Types
           , ExistentialQuantification
           , FunctionalDependencies
           , KindSignatures
           , ImplicitParams
           , OverlappingInstances
           , UndecidableInstances
           , TemplateHaskell
           , ScopedTypeVariables
           , GeneralizedNewtypeDeriving
           , ExplicitNamespaces #-}

module Data.Record where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Control.Monad
import Data.Monoid

-- | A key of a record. This does not exist at runtime, and as a tradeoff,
-- you can't do field access from a string and a Typeable context, although
-- it would certainly be very nice.
data Key k
data F a b = F a b
type (:=) = 'F

-- | Type composition
-- Used for cases where a record transformer has 
newtype (w :.: m) (x :: *) = Wmx { wmx :: w (m x) }
  deriving (Show, Eq, Ord, Enum)
infixr 9 :.:

data Pure
type family Wrap (w :: a) x
type instance Wrap (w :: * -> *) x = w x
type instance Wrap Pure x = x
data RecordT w r where 
    C :: Wrap w e -> RecordT w r -> RecordT w (k := e ': r)
    E :: RecordT w '[]
type Record = RecordT Pure

{-# INLINE (&) #-}
(&) :: Wrap w e -> RecordT w r -> RecordT w (k := e ': r)
(&) = C
infixr 4 &

end :: RecordT w '[]
end = E

--------------------------------------------------------------------------------
--  Standard instances

instance Eq (RecordT w '[]) where
    {-# INLINE (==) #-}
    _ == _ = True

instance ( Eq (Wrap w x)
         , Eq (RecordT w xs)) 
        => Eq (RecordT w (k := x ': xs)) where
    {-# INLINE (==) #-}
    C x xs == C y ys = x == y && xs == ys

instance Ord (RecordT w '[]) where
    {-# INLINE compare #-}
    compare _ _ = EQ

instance ( Ord (Wrap w x)
         , Ord (RecordT w xs))
        => Ord (RecordT w (k := x ': xs)) where
    {-# INLINE compare #-}
    compare (C x xs) (C y ys) = compare (compare x y) (compare xs ys)

instance Show (RecordT w '[]) where
    show _ = "end"

instance ( Show a
         , Show (Record xs)) 
        => Show (Record (k := a ': xs)) where
    show (C x xs) = show x ++ " & " ++ show xs

instance ( Show (w a)
         , Show (RecordT w xs)) 
        => Show (RecordT w (k := a ': xs)) where
    show (C x xs) = show x ++ " & " ++ show xs

instance Monoid (RecordT w '[]) where
    {-# INLINE mappend #-}
    {-# INLINE mempty  #-}
    mappend _ _  = end 
    mempty       = end

instance ( Monoid (Wrap w x)
         , Monoid (RecordT w xs)) 
        => Monoid (RecordT w (k := x ': xs)) where
    {-# INLINE mappend #-}
    mappend (C x xs) (C y ys) = mappend x y & mappend xs ys
    mempty = undefined -- impossible to reach anyway

--------------------------------------------------------------------------------
--  Field accessors/setters

class Access r k a | r k -> a where
    access :: Key k -> RecordT w r -> Wrap w a

instance Access (k := a ': xs) k a where
    {-# INLINE access #-}
    access _ (C x _) = x

instance Access xs k a => Access (k0 := a0 ': xs) k a where
    {-# INLINE access #-}
    access n (C _ xs) = access n xs

-- | Class to indicate whether a record "has" a key, and its type.
class Has r k a | r k -> a
instance Has k (k  := a  ': xs) (Just a)
instance Has k xs m => Has k (k0 := a0 ': xs) m

class Knock k r a | k r -> a where
    -- | Try ("knock politely") to get a field of a record.
    -- It's impossible to get proper "lookups" at runtime, so this function
    -- is probably not very useful.
    knock :: Key k -> RecordT w r -> Maybe (Wrap w a)

instance Has k r Nothing => Knock k r () where
    {-# INLINE knock #-}
    knock _ _ = Nothing

instance Access r k a => Knock k r a where
    {-# INLINE knock #-}
    knock k xs = Just (access k xs)

class Update r k a | r k -> a where
    -- | Write to a record's field
    write :: Key k -> Wrap w a  -> RecordT w r -> RecordT w r
    -- | Update a record's field
    alter :: Key k -> (Wrap w a -> Wrap w a) -> RecordT w r -> RecordT w r

instance Update (k := a ': xs) k a where
    {-# INLINE write #-}
    {-# INLINE alter #-}
    write _ x (C _ xs) = x   & xs
    alter _ f (C y ys) = f y & ys

instance Update xs k a => Update (k0 := a0 ': xs) k a where
    {-# INLINE write #-}
    {-# INLINE alter #-}
    write n y (C x xs) = x & write n y xs
    alter n f (C x xs) = x & alter n f xs

--------------------------------------------------------------------------------
--  Record combinators

class Box w m r wm | w m -> wm where
    -- | "Box" every element of a record.
    -- Usually means applying a newtype wrapper to everything
    box :: (forall a. Wrap m a -> w (Wrap m a)) -> RecordT m r -> RecordT wm r

instance Box w Pure '[] w where
    {-# INLINE box #-}
    box _ _ = end

instance Box w Pure xs w => Box w Pure (x ': xs) w where
    {-# INLINE box #-}
    box f (C x xs) = C (f x) (box f xs)

-- Compositions of the record wrapper types
instance Box w m '[] (w :.: m) where
    {-# INLINE box #-}
    box _ _ = end

instance Box (w :: * -> *) (m :: * -> *) xs (w :.: m) => Box w m (x ': xs) (w :.: m) where
    {-# INLINE box #-}
    box f (C x xs) = C (Wmx (f x)) (box f xs)


class Transform r where
    -- | Change the type wrapping every element of a record
    transform :: (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> RecordT i r -> RecordT o r

instance Transform '[] where
    {-# INLINE transform #-}
    transform _ _ = end

instance Transform xs => Transform (x ': xs) where
    {-# INLINE transform #-}
    transform f (C x xs) = f x & transform f xs

class Run r where
    -- | Iterate over a RecordT's elements, and use the inner monad to iterate
    -- over everything, and return a "pure" record.
    run :: Monad m => RecordT m r -> m (Record r)

instance Run '[] where
    run _ = return end

instance Run xs => Run (x ': xs) where
    run (C x xs) = liftM2 C x (run xs)

class Runtrans r where
    -- | Iterate over every element of a record. Logically similar to @ run . transform f @, but
    -- 'runtrans' should be more efficient.
    runtrans :: Monad o => (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> RecordT i r -> o (Record r)

instance Runtrans '[] where
    {-# INLINE runtrans #-}
    runtrans _ _ = return end

instance Runtrans xs => Runtrans (x ': xs) where
    {-# INLINE runtrans #-}
    runtrans f (C x xs) = liftM2 C (f x) (runtrans f xs)

--------------------------------------------------------------------------------
--  Subtyping

-- | Append two type-level lists
type family (++) (x :: [a]) (y :: [a]) :: [a]
type instance '[]       ++ '[]  = '[]
type instance '[]       ++  ys  = ys
type instance (x ': xs) ++  ys  = x ': (xs ++ ys)

class Append r0 r1 where
    -- | Make a record by appending 2.
    append :: RecordT w r0 -> RecordT w r1 -> RecordT w (r0 ++ r1)

instance Append '[] '[] where
    {-# INLINE append #-}
    append _ _ = end

instance Append '[] a where
    {-# INLINE append #-}
    append _ x = x

instance Append xs ys => Append (x ': xs) ys where
    {-# INLINE append #-}
    append (C x xs) ys = C x (append xs ys)

class Transrun r where
    transrun :: Monad m => (forall a. a -> m (w a)) -> Record r -> m (RecordT w r)

instance Transrun '[] where
    {-# INLINE transrun #-}
    transrun _ _ = return end

instance Transrun xs => Transrun (x ': xs) where
    {-# INLINE transrun #-}
    transrun f (C x xs) = liftM2 C (f x) (transrun f xs)

--------------------------------------------------------------------------------
--  Convenience QuasiQuoters

key :: String -> Q Exp
key s = [| undefined :: Key $(litT . return . StrTyLit $ s) |] 

-- | See 'write'
-- [set|x|] == write (undefined :: Key x)
set :: QuasiQuoter
set = QuasiQuoter { quoteExp = \s -> [| write $(key s) |], quoteType = undefined, quoteDec = undefined, quotePat = undefined }

-- | See 'alter'
-- > [alt|x|] == alter (undefined :: Key x)
alt :: QuasiQuoter
alt = QuasiQuoter { quoteExp = \s -> [| alter  $(key s) |], quoteType = undefined, quoteDec = undefined, quotePat = undefined }

-- | See 'access'.
-- > [get|x|] == access (undefined :: Key x)
get :: QuasiQuoter
get = QuasiQuoter { quoteExp = \s -> [| access $(key s) |], quoteType = undefined, quoteDec = undefined, quotePat = undefined }

