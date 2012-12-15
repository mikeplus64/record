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
           , OverlappingInstances
           , UndecidableInstances
           , TemplateHaskell
           , ScopedTypeVariables
           , ExplicitNamespaces #-}

module Data.Record ( key
                   , set
                   , alt
                   , get
                   , (&)
                   , end
                   , unbox
                   , box
                   , transform
                   , run
                   , runtrans
                   , access
                   , write
                   , alter
                   , append
                   , Record
                   , runcomp
                   , P
                   , (:=)
                   , type (++)
                   , Key ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Control.Category ((.))
import Control.Monad
import Prelude hiding ((.))

-- | A key of a record. This does not exist at runtime, and as a tradeoff,
-- you can't do field access from a string and a Typeable context, although
-- it would certainly be very nice.
data Key k

key  ::  String -> Q Exp
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

-- tuples are ugly
data F a b = F a b

type (:=) = 'F

data P

type family Wrap (w :: a) x
type instance Wrap (w :: * -> *) x = w x
type instance Wrap P x = x

data Record w r where 
    C :: Wrap w e -> Record w r -> Record w (k := e ': r)
    E :: Record w '[]

instance Show (Record w '[]) where
    show _ = "end"

instance (Show a, Show (Record P xs)) => Show (Record P (k := a ': xs)) where
    show (C x xs) = show x ++ " & " ++ show xs

instance (Show (w a), Show (Record w xs)) => Show (Record w (k := a ': xs)) where
    show (C x xs) = show x ++ " & " ++ show xs

(&) :: Wrap w e -> Record w r -> Record w (k := e ': r)
(&) = C
infixr 4 &

end :: Record w '[]
end = E

class Unbox r where 
    -- | "Unbox" every element of a record.
    -- Great for cases where every element is wrapped by a newtype.
    unbox :: (forall a. w a -> a) -> Record (w :: * -> *) r -> Record P r 

instance Unbox '[] where 
    {-# INLINE unbox #-}
    unbox _ _ = end

instance Unbox xs => Unbox (x ': xs) where
    {-# INLINE unbox #-}
    unbox f (C x xs) = f x & unbox f xs

class Box r where
    -- | "Box" every element of a record.
    -- Usually means applying a newtype wrapper to everything
    box :: (forall a. a -> w a) -> Record P r -> Record (w :: * -> *) r

instance Box '[] where
    {-# INLINE box #-}
    box _ _ = end

instance Box xs => Box (x ': xs) where
    {-# INLINE box #-}
    box f (C x xs) = C (f x) (box f xs)

class Transform r where
    -- | Change the type wrapping every element of a record
    transform :: (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> Record i r -> Record o r

instance Transform '[] where
    {-# INLINE transform #-}
    transform _ _ = end

instance Transform xs => Transform (x ': xs) where
    {-# INLINE transform #-}
    transform f (C x xs) = f x & transform f xs

class Run r where
    -- | Iterate over a Record's elements, and use a monad to unbox them
    -- Especially handy in situations like transforming a @Record IORef a@ to 
    -- @IO (Record P a)@, where you can simply use run . transform readIORef
    run :: Monad m => Record m r -> m (Record P r)

instance Run '[] where
    run _ = return end

instance (Run xs) => Run (x ': xs) where 
    run (C x xs) = liftM2 C x (run xs)

class Runtrans r where
    -- | A more efficient implementation of @ run . transform f @.
    -- Rewrite rules should transform @ run . transform f @ into a call
    -- to @ runtrans f @
    runtrans :: Monad o => (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> Record i r -> o (Record P r)

instance Runtrans '[] where
    {-# INLINE runtrans #-}
    runtrans _ _ = return end

instance Runtrans xs => Runtrans (x ': xs) where
    {-# INLINE runtrans #-}
    runtrans f (C x xs) = liftM2 C (f x) (runtrans f xs)

class Access r k a | r k -> a where
    access :: Key k -> Record w r -> Wrap w a

instance Access (k := a ': xs) k a where
    {-# INLINE access #-}
    access _ (C x _) = x

instance Access xs k a => Access (k0 := a0 ': xs) k a where
    {-# INLINE access #-}
    access n (C _ xs) = access n xs

class Update r k a | r k -> a where
    -- | Write to a record's field
    write :: Key k -> Wrap w a  -> Record w r -> Record w r
    -- | Update a record's field
    alter :: Key k -> (Wrap w a -> Wrap w a)  -> Record w r -> Record w r

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

-- | Append two type-level lists
type family (++) (x :: [a]) (y :: [a]) :: [a]
type instance '[]       ++ '[]  = '[]
type instance '[]       ++  ys  = ys
type instance (x ': xs) ++  ys  = x ': (xs ++ ys)

class Append r0 r1 where
    -- | Make a record by appending 2.
    append :: Record w r0 -> Record w r1 -> Record w (r0 ++ r1)

instance Append '[] '[] where
    {-# INLINE append #-}
    append _ _ = end

instance Append '[] a where
    {-# INLINE append #-}
    append _ x = x

instance Append xs ys => Append (x ': xs) ys where
    {-# INLINE append #-}
    append (C x xs) ys = C x (append xs ys)

class RunComp r where
    runcomp :: (Functor m, Monad m) => (forall a. a -> m (w a)) -> Record P r -> m (Record w r)

instance RunComp '[] where
    {-# INLINE runcomp #-}
    runcomp _ _ = return end

instance RunComp xs => RunComp (x ': xs) where
    {-# INLINE runcomp #-}
    runcomp f (C x xs) = liftM2 C (f x) (runcomp f xs)

