{-# LANGUAGE GADTs, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ConstraintKinds, DataKinds, TypeOperators, PolyKinds, EmptyDataDecls, Rank2Types, ExistentialQuantification, FunctionalDependencies #-}
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

-----------------------------------------------------------------------------
-- RecordT classes and types

data RecordT ::  (* -> *)      -- ^ type constructor to wrap values
             -> [ F Symbol * ] -- ^ type-level key/value list
             -> * where

    Et :: RecordT m '[]
    Ct :: m a -> RecordT m xs -> RecordT m (k := a ': xs)
infixr 4 `Ct`

type family Field (k :: Symbol) (u :: * -> *) (r :: [ F Symbol * ]) :: [ F Symbol * ]
type instance Field k u '[] = '[]
type instance Field k u (k := a ': xs) = k := u a ': Field k u xs

class Transform r where
    transform :: (forall a. i a -> o a) -> RecordT i r -> RecordT o r

instance Transform '[] where
    {-# INLINE transform #-}
    transform _ _ = Et

instance Transform xs => Transform (x ': xs) where 
    {-# INLINE transform #-}
    transform f (Ct x xs) = Ct (f x) (transform f xs)

class TransformTypeable r where
    transformTyp :: (forall a. Typeable a => i a -> o a) -> RecordT i r -> RecordT o r

instance TransformTypeable '[] where
    transformTyp _ _ = Et

instance (Typeable a, TransformTypeable xs) => TransformTypeable (k := a ': xs) where
    transformTyp f (Ct x xs) = Ct (f x) (transformTyp f xs)


instance Show (RecordT m '[]) where
    show _ = "End"

instance (Show (m a), Show (RecordT m xs)) => Show (RecordT m (k := a ': xs)) where
    show (Ct x xs) = show x ++ " ::: " ++ show xs


class Run m r where
    run :: RecordT m r -> m (Record r)

instance Monad m => Run m '[] where 
    {-# INLINE run #-}
    run _ = return E

instance (Monad m, Run m xs) => Run m (x ': xs) where
    {-# INLINE run #-}
    run (Ct x xs) = do
        y  <- x
        ys <- run xs
        return (C y ys)


class AccessT k r a | k r -> a where
    accessT :: RecordT m r -> N k -> m a

instance AccessT k (k := a ': xs) a where
    {-# INLINE accessT #-}
    accessT (Ct x _) _ = x

instance AccessT k xs a => AccessT k (ka0 ': xs) a where
    {-# INLINE accessT #-}
    accessT (Ct _ x) n = accessT x n

type IORec = RecordT IORef
type Ball  = '[ "x" := Int, "y" := Int, "z" := Int, "dx" := Int ]


ib :: IO (IORec Ball)
ib = do
    x  <- newIORef 0
    y  <- newIORef 0
    z  <- newIORef 0
    dx <- newIORef 0
    return (x `Ct` y `Ct` z `Ct` dx `Ct` Et)

-----------------------------------------------------------------------------
-- Record classes and types

data Record :: [ F Symbol * ] -> * where
    E :: Record '[]
    C :: a -> Record xs -> Record (k := a ': xs)

instance Show (Record '[]) where
    show _ = "End"
instance (Show a, Show (Record xs)) => Show (Record (k := a ': xs)) where
    show (C x xs) = show x ++ " ::: " ++ show xs




