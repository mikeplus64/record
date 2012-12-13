{-# LANGUAGE GADTs, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ConstraintKinds, DataKinds, TypeOperators, PolyKinds, EmptyDataDecls, Rank2Types, ExistentialQuantification, FunctionalDependencies, KindSignatures #-}
import GHC.TypeLits
import Control.Monad.Identity
import Data.IORef
import Data.STRef
import Data.List (intercalate)
import Data.Typeable

data N k

na :: N Symbol
na = undefined

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
    unbox :: (forall a. w a -> a) -> Record (w :: * -> *) r -> Record P r 
instance Unbox '[] where 
    unbox _ _ = end
instance Unbox xs => Unbox (x ': xs) where
    unbox f (Ct x xs) = f x & unbox f xs

class Transform r where
    transform :: (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> Record i r -> Record o r
instance Transform '[] where
    transform _ _ = end
instance Transform xs => Transform (x ': xs) where
    transform f (Ct x xs) = f x & transform f xs

-- | Iterate over a Record's elements, and use a monad to unbox them
-- Especially handy in situations like transforming a @Record IORef a@ to 
-- @IO (Record P a)@, where you can simply use run . transform readIORef
class Run r where
    run :: Monad m => Record m r -> m (Record P r)
instance Run '[] where
    run _ = return end
instance Run xs => Run (x ': xs) where
    run (Ct x xs) = do
        y  <- x
        ys <- run xs
        return (y & ys)

-- | A more efficient implementation of @ run . transform f @.
-- Not exported because it is much hairier to use than that, but
-- rewrite rules will transform @ run . transform f @ into a call
-- to @ runtrans f @
class Runtrans r where
    runtrans :: Monad o => (forall a. (i :: * -> *) a -> (o :: * -> *) a) -> Record i r -> o (Record P r)
instance Runtrans '[] where
    runtrans _ _ = return end
instance Runtrans xs => Runtrans (x ': xs) where
    runtrans f (Ct x xs) = do
        y  <- f x
        ys <- runtrans f xs
        return (y & ys)

{-# RULES 
    "Record/runtrans" 
    forall (f :: forall a. i a -> o a) (r :: Runtrans r => Record i r). run (transform f r) = runtrans f r           
  #-}


