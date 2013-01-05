--------------------------------------------------------------------------------
-- | 
-- Module      : Data.Record
-- Copyright   : 2012-2013 Mike Ledger
-- License     : BSD3
-- Maintainer  : eleventynine@gmail.com
-- Stability   : experimental
-- 
-- Data.Record provides a "record transformer" -- implemented as a heterogenous 
-- linked list similar to HList. Data.Record records should have no more 
-- overhead than actual linked lists, as accessors/keys only exist at the type 
-- level, which also helps to ensure safety. This module provides some
-- convenience 'QuasiQuoter's for syntactically easier record updates/accesses.
-- 
-- TODO:
--  * Try and make 'Typeable' and 'Data' instances.
--  * Fix unions to actually make unions of the records instead of
--    not working (at compile time) when records have duplicate keys
--------------------------------------------------------------------------------

-- and in no particular order ...
{-# LANGUAGE GADTs
           , ImplicitParams
           , TypeFamilies
           , ConstraintKinds
           , FlexibleInstances
           , DataKinds
           , TypeOperators
           , PolyKinds
           , EmptyDataDecls
           , RankNTypes
           , FunctionalDependencies
           , KindSignatures
           , OverlappingInstances
           , UndecidableInstances
           , TemplateHaskell
           , ScopedTypeVariables
           , GeneralizedNewtypeDeriving
           , ExplicitNamespaces #-}

module Data.Record 
  ( Record
  , RecordT

  -- * Construction
  , (&)
  , (&-)
  , nil
  , Identity(..)

  , Key
  , (:=)

  -- * Field accessing
  , Access(..)
  , Has
  , Knock(..)

  -- * Field updates
  , Update(..)
  
  -- * Field deletion
  , Delete(..)

  -- * Transformations
  , Box(..)
  , Transform(..)
  , Run(..)
  , Runtrans(..)
  , Transrun(..)
  
  -- * Unions
  , type (++)
  , Union(..)
  , CombineWith(..)
  , AllUnique
  , IsElem

  -- * Records with "deeper" transformations
  , type (:.:)
  , compose

  -- * Convenience
  , Symbol
  , key
  , is
  , alt
  , fields
  , fieldr
  ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import GHC.TypeLits

instance Show a => Show (Identity a) where show = show . runIdentity

-- | A key of a record. This does not exist at runtime, and as a tradeoff,
-- you can't do field access from a string and a Typeable context, although
-- it would certainly be very nice.
data Key k
data F a b = F a b
type (:=) = 'F

-- | Type composition
-- Used for cases where a record transformer is "deeper" than normal, e.g. for
-- functions like 'newIORef :: a -> IO (IORef a)'
newtype (w :.: m) (x :: *) = Wmx (w (m x))
  deriving (Show, Eq, Ord, Enum)

infixr 9 :.:

compose :: (a -> w (m a)) -> a -> (w :.: m) a
compose f = Wmx . f

-- | The base record transformer data type. Fields are indexed by type-level
-- keys, which can be anything. It is very convenient to use
-- 'GHC.TypeLits.Symbol' to index record fields, but it is just as valid to
-- declare phantom types for them.
data RecordT w r where 
    C :: w e -> RecordT w r -> RecordT w (k := e ': r)
    E :: RecordT w '[]

type Record = RecordT Identity

{-# INLINE (&) #-}
(&) :: e -> Record r -> Record (k := e ': r)
(&) = C . Identity
infixr 5 &

{-# INLINE (&-) #-}
(&-) :: w e -> RecordT w r -> RecordT w (k := e ': r)
(&-) = C
infixr 5 &-

nil :: RecordT w '[]
nil = E

--------------------------------------------------------------------------------
--  Standard instances

instance Eq (RecordT w '[]) where
    {-# INLINE (==) #-}
    _ == _ = True

instance ( Eq (w x)
         , Eq (RecordT w xs)) 
        => Eq (RecordT w (k := x ': xs)) where
    {-# INLINE (==) #-}
    C x xs == C y ys = x == y && xs == ys

instance Ord (RecordT w '[]) where
    {-# INLINE compare #-}
    compare _ _ = EQ

instance ( Ord (w x)
         , Ord (RecordT w xs))
        => Ord (RecordT w (k := x ': xs)) where
    {-# INLINE compare #-}
    compare (C x xs) (C y ys) = compare (compare x y) (compare xs ys)

instance Show (RecordT w '[]) where
    show _ = "nil"

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
    mappend _ _  = nil
    mempty       = nil

instance ( Monoid (w x)
         , Monoid (RecordT w xs)) 
        => Monoid (RecordT w (k := x ': xs)) where
    {-# INLINE mappend #-}
    mappend (C x xs) (C y ys) = mappend x y     `C` mappend xs ys
    mempty                    = (mempty :: w x) `C` mempty

--------------------------------------------------------------------------------
--  Field accessors/setters

class Access r k a | r k -> a where
    access :: Key k -> RecordT w r -> w a

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
    knock :: Key k -> RecordT w r -> Maybe (w a)

instance Has k r Nothing => Knock k r () where
    {-# INLINE knock #-}
    knock _ _ = Nothing

instance Access r k a => Knock k r a where
    {-# INLINE knock #-}
    knock k xs = Just (access k xs)

class Update r k a | r k -> a where
    -- | Write to a record's field
    write :: Key k -> w a  -> RecordT w r -> RecordT w r
    -- | Update a record's field
    alter :: Key k -> (w a -> w a) -> RecordT w r -> RecordT w r

instance Update (k := a ': xs) k a where
    {-# INLINE write #-}
    {-# INLINE alter #-}
    write _ x (C _ xs) = x   `C` xs
    alter _ f (C y ys) = f y `C` ys

instance Update xs k a => Update (k0 := a0 ': xs) k a where
    {-# INLINE write #-}
    {-# INLINE alter #-}
    write n y (C x xs) = x `C` write n y xs
    alter n f (C x xs) = x `C` alter n f xs


--------------------------------------------------------------------------------
--  Field deletion

class Delete r0 r1 k | r0 k -> r1 where
    delete :: Key k -> RecordT w r0 -> RecordT w r1

instance Delete '[] '[] k where
    delete _ _ = nil

instance Delete (k := x ': xs) xs k where
    delete _ (C _ xs) = xs

instance Delete xs ys k => Delete (k0 := x ': xs) (k0 := x ': ys) k where
    delete k (C x xs) = C x (delete k xs)

--------------------------------------------------------------------------------
--  Record combinators

class Box w m r where
    -- | "Box" every element of a record.
    -- Usually means applying a newtype wrapper to everything
    box :: (forall a. m a -> w (m a)) -> RecordT m r -> RecordT (w :.: m) r

-- Compositions of the record wrapper types
instance Box w m '[] where
    {-# INLINE box #-}
    box _ _ = nil

instance Box (w :: * -> *) (m :: * -> *) xs => Box w m (x ': xs) where
    {-# INLINE box #-}
    box f (C x xs) = C (Wmx (f x)) (box f xs)


class Transform r where
    -- | Change the type wrapping every element of a record
    transform :: (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> RecordT i r -> RecordT o r

instance Transform '[] where
    {-# INLINE transform #-}
    transform _ _ = nil

instance Transform xs => Transform (x ': xs) where
    {-# INLINE transform #-}
    transform f (C x xs) = f x `C` transform f xs

class Run r where
    -- | Iterate over a RecordT's elements, and use the inner monad to iterate
    -- over everything, and return a "pure" record.
    run :: Monad m => RecordT m r -> m (Record r)

instance Run '[] where
    {-# INLINE run #-}
    run _ = return nil

instance Run xs => Run (x ': xs) where
    {-# INLINE run #-}
    run (C x xs) = liftM2 (&) x (run xs)

class Runtrans r where
    -- | Iterate over every element of a record. Logically similar to @ run . transform f @, but
    -- 'runtrans' should be more efficient.
    runtrans :: Monad o => (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> RecordT i r -> o (Record r)

instance Runtrans '[] where
    {-# INLINE runtrans #-}
    runtrans _ _ = return nil

instance Runtrans xs => Runtrans (x ': xs) where
    {-# INLINE runtrans #-}
    runtrans f (C x xs) = liftM2 (&) (f x) (runtrans f xs)

class Transrun r where
    transrun :: Monad m => (forall a. a -> m (w a)) -> Record r -> m (RecordT w r)

instance Transrun '[] where
    {-# INLINE transrun #-}
    transrun _ _ = return nil

instance Transrun xs => Transrun (x ': xs) where
    {-# INLINE transrun #-}
    transrun f (C x xs) = liftM2 C (f (runIdentity x)) (transrun f xs)

--------------------------------------------------------------------------------
--  Unions

-- | Append two type-level lists
type family (++) (x :: [a]) (y :: [a]) :: [a]
type instance '[]       ++ '[]  = '[]
type instance '[]       ++  ys  = ys
type instance (x ': xs) ++  ys  = x ': (xs ++ ys)

class AllUnique (r0 :: [F key *]) (r1 :: [F key *])
instance (IsElem r1 k False, AllUnique r0 r1) => AllUnique (k := a ': r0) r1
instance AllUnique '[] r1
instance AllUnique r0 '[]

class IsElem r k a | r k -> a
instance IsElem '[]              k False
instance IsElem (k  := a  ': xs) k True
instance IsElem xs k m => IsElem (k0 := a0 ': xs) k m

class Union r0 r1 where
    -- | Merge a record. Unfortunately at the moment records that share fields
    -- are simply not accepted. I'm not sure how to write a type family to
    -- really make a "union".
    union :: AllUnique r0 r1 => RecordT w r0 -> RecordT w r1 -> RecordT w (r0 ++ r1)

instance Union '[] '[] where
    {-# INLINE union #-}
    union _ _ = nil

instance Union '[] a where
    {-# INLINE union #-}
    union _ x = x

instance (AllUnique xs ys, Union xs ys) => Union (x ': xs) ys where
    {-# INLINE union #-}
    union (C x xs) ys = C x (union xs ys)

class CombineWith r where
    -- | Take two records with identical fields, and "combine" them with some combining function.
    -- e.g. @ combineWith (<|>) (r0 :: RecordT Maybe r) (r1 :: RecordT Maybe r) @
    combineWith :: (forall (a :: *). w a -> w a -> w a) -> RecordT w r -> RecordT w r -> RecordT w r

instance CombineWith '[] where
    combineWith _ _ _ = nil

instance CombineWith xs => CombineWith (x ': xs) where
    combineWith f (C x xs) (C y ys) = C (f x y) (combineWith f xs ys)

--------------------------------------------------------------------------------
--  Convenience QuasiQuoters

kq :: String -> Q Exp
kq s = [| undefined :: Key $(litT . return . StrTyLit $ s) |] 

key :: QuasiQuoter
key = QuasiQuoter { quoteExp = kq, quoteType = undefined, quoteDec = undefined, quotePat = undefined }


--------------------------------------------------------------------------------
--  Monad transformer convenience operators

is :: (MonadState (RecordT w r) m, Update r k a) => Key k -> w a -> m ()
is k a = modify (write k a)
infixr 1 `is`

alt :: (MonadState (RecordT w r) m, Update r k a) => Key k -> (w a -> w a) -> m ()
alt k f = modify (alter k f)
infixr 1 `alt`

fields :: (MonadState (RecordT w r) m, Access r k a) => Key k -> m (w a)
fields = gets . access

fieldr :: (MonadReader (RecordT w r) m, Access r k a) => Key k -> m (w a)
fieldr = asks . access
