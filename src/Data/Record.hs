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
                   , P
                   , (:=)
                   , NotElem
                   , AllNotElem
                   , type (++)
                   , Keys ) where

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
data family Record (t :: a) (r :: [F Symbol *])
-- | "Pure" records
data instance Record (w :: *) r where 
    Cp :: e -> Record P r -> Record P (k := e ': r)
    Ep :: Record P '[]
-- | Record transformer
data instance Record (w :: * -> *) r where 
    Ct :: w e -> Record w r -> Record w (k := e ': r)
    Et :: Record (w :: * -> *) '[]

instance Show (Record P '[]) where
    show _ = "end"
instance (Show a, Show (Record P xs)) => Show (Record P (k := a ': xs)) where
    show (Cp x xs) = show x ++ " & " ++ show xs

instance Show (Record w '[]) where
    show _ = "end"
instance (Show (w a), Show (Record w xs)) => Show (Record w (k := a ': xs)) where
    show (Ct x xs) = show x ++ " & " ++ show xs

class BuildRecord w where
    type Val w e
    (&) :: Val w e -> Record w r -> Record w (k := e ': r)
    end :: Record w '[]
instance BuildRecord P where
    type Val P e = e
    {-# INLINE (&) #-}
    {-# INLINE end #-}
    (&) = Cp
    end = Ep
instance BuildRecord (w :: * -> *) where
    type Val w e = w e
    {-# INLINE (&) #-}
    {-# INLINE end #-}
    (&) = Ct
    end = Et

class Unbox r where 
    -- | "Unbox" every element of a record.
    -- Great for cases where every element is wrapped by a newtype.
    unbox :: (forall a. w a -> a) -> Record (w :: * -> *) r -> Record P r 
instance Unbox '[] where 
    {-# INLINE unbox #-}
    unbox _ _ = end
instance Unbox xs => Unbox (x ': xs) where
    {-# INLINE unbox #-}
    unbox f (Ct x xs) = f x & unbox f xs

class Box r where
    -- | "Box" every element of a record.
    -- Usually means applying a newtype wrapper to everything
    box :: (forall a. a -> w a) -> Record P r -> Record (w :: * -> *) r
instance Box '[] where
    box _ _ = end
instance Box xs => Box (x ': xs) where
    box f (Cp x xs) = Ct (f x) (box f xs)

class Transform r where
    -- | Change the type wrapping every element of a record
    transform :: (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> Record i r -> Record o r
instance Transform '[] where
    transform _ _ = end
instance Transform xs => Transform (x ': xs) where
    transform f (Ct x xs) = f x & transform f xs

class Run r where
    -- | Iterate over a Record's elements, and use a monad to unbox them
    -- Especially handy in situations like transforming a @Record IORef a@ to 
    -- @IO (Record P a)@, where you can simply use run . transform readIORef
    run :: Monad m => Record m r -> m (Record P r)
instance Run '[] where
    run _ = return end
instance Run xs => Run (x ': xs) where
    run (Ct x xs) = do
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
    runtrans f (Ct x xs) = do
        y  <- f x
        ys <- runtrans f xs
        return (y & ys)
{-# RULES "Record/runtrans" forall (f :: forall a. i a -> o a) (r :: Runtrans r => Record i r). run (transform f r)   = runtrans f r #-}

class Access w r k a | w r k -> a where
    -- | Get a field of a record given its label.
    access :: Key k -> Record w r -> a
instance Access P (k := a ': xs) k a where
    {-# INLINE access #-}
    access _  (Cp x _) = x
instance Access P xs k a => Access P (k0 := a0 ': xs) k a where
    {-# INLINE access #-}
    access n (Cp _ xs) = access n xs
instance Access (w :: * -> *) (k := a ': xs) k (w a) where
    {-# INLINE access #-}
    access _  (Ct x _) = x
instance Access (w :: * -> *) xs k (w a) => Access (w :: * -> *) (k0 := a0 ': xs) k (w a) where
    {-# INLINE access #-}
    access n (Ct _ xs) = access n xs

class Update w r k a | r k -> a where
    -- | Write to a record's field
    write :: Key k -> a        -> Record w r -> Record w r
    -- | Update a record's field
    alter :: Key k -> (a -> a) -> Record w r -> Record w r
instance Update P (k := a ': xs) k a where
    {-# INLINE write #-}
    {-# INLINE alter #-}
    write _ x (Cp _ xs) = x   & xs
    alter _ f (Cp y ys) = f y & ys
instance Update P xs k a => Update P (k0 := a0 ': xs) k a where
    {-# INLINE write #-}
    {-# INLINE alter #-}
    write n y (Cp x xs) = x & write n y xs
    alter n f (Cp x xs) = x & alter n f xs

class NotElem (x :: a) (xs :: [a])
instance NotElem y '[ x ]
instance NotElem y xs => NotElem y (x ': xs)

class AllNotElem (xs :: [a]) (ys :: [a])
instance AllNotElem '[] ys
instance AllNotElem xs '[]
instance (NotElem y xs, AllNotElem ys xs) => AllNotElem (y ': ys) xs

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
    append (Cp x xs) ys = Cp x (append xs ys)
instance (AllNotElem (Keys xs) (Keys ys), Append (w :: * -> *) xs ys) => Append w (x ': xs) ys where
    append (Ct x xs) ys = Ct x (append xs ys)

