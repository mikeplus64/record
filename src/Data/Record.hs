-- | Type safe labelled records implemented using GADTs, type literals, and other fun stuff.
-- Functions that use TH are for convenience only -- they are only there to save you from typing a few extra characters
--
-- Extensions you need to use this module are: TypeOperators, DataKinds

{-# LANGUAGE Rank2Types, UndecidableInstances, OverlappingInstances, TypeFamilies, RecordWildCards, DataKinds, TypeOperators, FunctionalDependencies, GADTs, KindSignatures, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, PolyKinds, EmptyDataDecls #-}
module Data.Record where
{-
module Data.Record 
    ( n
    , N
    , key
    , type (++)
    , Record(..)
    , update
    , write
    , access
    , (!)
    , (=:)
    , (::=)
    , type (&) ) where
-}

import GHC.TypeLits hiding (type (*))
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

default(Integer)

data N :: Symbol -> *

-- | Convenience. @ n = undefined @
n :: N k
n = undefined

get, set, alt, key :: QuasiQuoter
get = QuasiQuoter { quoteExp = \s -> [| flip access (n :: N $(clean s)) |] }
set = QuasiQuoter { quoteExp = \s -> [| write  (n :: N $(clean s)) |] }
alt = QuasiQuoter { quoteExp = \s -> [| update (n :: N $(clean s)) |] }
key = QuasiQuoter { quoteExp = \s -> [| n :: N $(clean s) |] }

clean :: String -> TypeQ
clean = litT . strTyLit . unwords . words

data Record :: [(Symbol, *)] -> * where 
    End   :: Record '[]
    (:::) :: a -> Record xs -> Record ( '(n, a) ': xs)

type (k :: Symbol) ::= a = '(k, a)

infixl 8 ::=
infixr 3 :::

-- Instances
-- if only these could be derived automatically ...
--
-- TODO: Typeable and Data
-- frankly I have no idea how to approach either, since Record's paramater isn't *

instance Show (Record '[]) where
    show _ = "End"

instance (Show a, Show (Record xs)) => Show (Record (k ::= a ': xs)) where
    show (x ::: xs) = show x ++ " ::: " ++ show xs

instance Eq (Record '[]) where
    _ == _ = True

instance (Eq a, Eq (Record xs)) => Eq (Record (k ::= a ': xs)) where
    (x ::: xs) == (y ::: ys) = x == y && xs == ys

instance Ord (Record '[]) where
    compare _ _ = EQ

instance (Ord a, Ord (Record xs)) => Ord (Record (k ::= a ': xs)) where
    compare (x ::: xs) (y ::: ys) = compare (compare x y) (compare xs ys)

class Get (r :: [(Symbol, *)]) (k :: Symbol) a | r k -> a where
    -- | Get a field of a record.
    access :: Record r -> N k -> a

instance Get (k ::= a ': xs) k a where
    access (a ::: _) _ = a

instance Get xs k a => Get (k1 ::= a1 ': xs) k a where
    access (_ ::: a) f = access a f

class Write (r :: [(Symbol, *)]) (k :: Symbol) a | r k -> a where
    write :: N k -> a -> Record r -> Record r
    update :: N k -> (a -> a) -> Record r -> Record r

instance Write (k ::= a ': xs) k a where
    write _ x (_ ::: b) = x ::: b
    update _ f (x ::: b) = f x ::: b

instance Write xs k a => Write (k1 ::= a1 ': xs) k a where
    write f x (a ::: b) = a ::: write f x b
    update l f (a ::: b) = a ::: update l f b

(!) :: Get r k a => Record r -> N k -> a
(!) = access

(=:) :: Write r k a => N k -> a -> Record r -> Record r
(=:) = write

<<<<<<< HEAD
-- | Append type lists
type family (++) (x :: [a]) (y :: [a]) :: [a]
type instance '[]       ++ xs = xs
type instance (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Get a record's size at compile time
-- 
size :: Name -> Q Exp
size z = reify z >>= getType >>= fmap lift go
  where 
        realType (AppT (ConT _) (ConT qt)) = do
            r <- reify qt
            case r of
                TyConI (TySynD _ _ t) -> return t
                _                     -> error (show r)

        realType t = error (show t)

        getType (VarI _ t _ _) = realType t
        getType x              = error (show x)

        go :: Type -> Int
        go PromotedNilT           = 0
        go (AppT PromotedConsT i) = 1 + go i
        go (AppT q             i) = go q + go i
        go _                      = 0

=======
class Append xs ys where
    -- | Create a new record by appending two, useful for inheritence / subtyping.
    (&) :: Record xs -> Record ys -> Record (xs ++ ys)
infixr 2 &

instance Append '[] ys where
    _ & ys = ys

instance Append xs ys => Append (x ': xs) ys where
    (x ::: xs) & ys = x ::: (xs & ys)

-- | Use (:+) as 'cons' for the record fields.
type x :+ xs = x ': xs
infixr 4 :+

type family (++) (x :: [a]) (y :: [a]) :: [a]
type instance '[]       ++ xs = xs
type instance (x ': xs) ++ ys = x ': (xs ++ ys)
>>>>>>> 55ab3b751cb4cb5e78dd3fdf8016f7ac28910c9c
