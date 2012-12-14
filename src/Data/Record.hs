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
                --   , append
                   , Record
                   , P
                   , (:=)
               --    , NotElem
                 --  , AllNotElem
                   , type (++)
                   , Keys ) where
module Data.Record where
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import GHC.TypeLits

-- | A key of a record. This does not exist at runtime, and as a tradeoff,
-- you can't do field access from a string and a Typeable context, although
-- it would certainly be very nice.
data Key k

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

-- | A field
data F a b = F a b

type (:=) = 'F

infixr 4 &

data P

type family Wrap (w :: a) x
type instance Wrap (w :: * -> *) x = w x
type instance Wrap P             x = x

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
    box _ _ = end
instance Box xs => Box (x ': xs) where
    box f (C x xs) = C (f x) (box f xs)

class Transform r where
    -- | Change the type wrapping every element of a record
    transform :: (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> Record i r -> Record o r
instance Transform '[] where
    transform _ _ = end
instance Transform xs => Transform (x ': xs) where
    transform f (C x xs) = f x & transform f xs

class Run r where
    -- | Iterate over a Record's elements, and use a monad to unbox them
    -- Especially handy in situations like transforming a @Record IORef a@ to 
    -- @IO (Record P a)@, where you can simply use run . transform readIORef
    run :: Monad m => Record m r -> m (Record P r)
instance Run '[] where
    run _ = return end
instance Run xs => Run (x ': xs) where
    run (C x xs) = do
        y  <- x
        ys <- run xs
        return (y & ys)

class Runtrans r where
    -- | A more efficient implementation of @ run . transform f @.
    -- Rewrite rules should transform @ run . transform f @ into a call
    -- to @ runtrans f @
    runtrans :: Monad o => (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> Record i r -> o (Record P r)
instance Runtrans '[] where
    runtrans _ _ = return end
instance Runtrans xs => Runtrans (x ': xs) where
    runtrans f (C x xs) = do
        y  <- f x
        ys <- runtrans f xs
        return (y & ys)

{-# RULES "Record/runtrans" 
          forall (f :: forall a. i a -> o a) (r :: Runtrans r => Record i r). 
          run (transform f r) = runtrans f r #-}

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

{-
class Change w r k where
    type New w r k a
    writep :: Key k -> a -> Record w r -> Record w (New w r k a)


class Is a b (t :: Bool)
instance Is a a 'True
instance Is a b 'False

class NotElem a as
instance (NotElem a bs, Is a b 'False) => NotElem a (b ': bs)
instance NotElem a '[]

class AllNotElem xs ys
instance AllNotElem '[] ys
instance (NotElem x ys, AllNotElem xs ys) => AllNotElem (x ': xs) ys

type family Not (a :: Bool) :: Bool
type instance Not 'True  = 'False
type instance Not 'False = 'True

type family Keys (xs :: [F k a]) :: [k]
type instance Keys '[] = '[]
type instance Keys (k := a ': xs) = k ': Keys xs

-- | Append two type-level lists
type family (++) (x :: [a]) (y :: [a]) :: [a]
type instance '[]       ++ ys  = ys
type instance (x ': xs) ++ ys  = x ': (xs ++ ys)

class Append w r0 r1 where
    -- | Append two records, making sure first that there are no duplicate fields
    append :: AllNotElem (Keys r0) (Keys r1) => Record w r0 -> Record w r1 -> Record w (r0 ++ r1)
instance Append w '[] ys where
    append _ ys = ys
instance (AllNotElem (Keys xs) (Keys ys), Append P xs ys) => Append P (x ': xs) ys where
    append (C x xs) ys = C x (append xs ys)
instance (AllNotElem (Keys xs) (Keys ys), Append (w :: * -> *) xs ys) => Append w (x ': xs) ys where
    append (C x xs) ys = C x (append xs ys)
-}
