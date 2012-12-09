-- | Simple, type safe labelled records implemented using GADTs, type literals, and other fun stuff.
-- Functions that use TH are for convenience only -- they are only there to save you from typing a few extra characters
--
-- Extensions you need to use this module are: TypeOperators, DataKinds
--

{-# LANGUAGE UndecidableInstances, OverlappingInstances, TypeFamilies, RecordWildCards, DataKinds, TypeOperators, FunctionalDependencies, GADTs, KindSignatures, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, PolyKinds, EmptyDataDecls, ScopedTypeVariables #-}
module Data.Record 
    ( n
    , N

    , update
    , access
    , modify
    , write

    , set
    , upd
    , alt
    , modi
    , get

    , type (++)
    , Record(..)
    , (:+)
    , (::=)
    , (+++) ) where

import GHC.TypeLits
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

data N :: Symbol -> *

-- | Convenience. @ n = undefined @
n :: N k
n = undefined

clean :: String -> TypeQ
clean = litT . strTyLit . unwords . words

-- | See 'write'.
set :: QuasiQuoter
set = QuasiQuoter{ quoteExp = \s -> [|write (undefined :: N $(clean s))|] }

-- | See 'update'
upd :: QuasiQuoter
upd = QuasiQuoter{ quoteExp = \s -> [|update (undefined :: N $(clean s))|] }

-- | See 'alter'
alt :: QuasiQuoter
alt = QuasiQuoter{ quoteExp = \s -> [|alter (undefined :: N $(clean s))|] }

-- | See 'modify'
modi :: QuasiQuoter
modi = QuasiQuoter{ quoteExp = \s -> [|modify (undefined :: N $(clean s))|] }

-- | See 'access'
get :: QuasiQuoter
get = QuasiQuoter{ quoteExp = \s -> [|flip access (undefined :: N $(clean s))|] }

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
-- frankly I have no idea how to approach either

instance Show (Record '[]) where
    show _ = "End"

instance (Show a, Show (Record xs)) => Show (Record ( '(k,a) ': xs)) where
    show (x ::: xs) = show x ++ " ::: " ++ show xs

instance Eq (Record '[]) where
    _ == _ = True

instance (Eq a, Eq (Record xs)) => Eq (Record ('(k, a) ': xs)) where
    (x ::: xs) == (y ::: ys) = x == y && xs == ys

instance Ord (Record '[]) where
    compare _ _ = EQ

instance (Ord a, Ord (Record xs)) => Ord (Record ('(k,a) ': xs)) where
    compare (x ::: xs) (y ::: ys) = compare (compare x y) (compare xs ys)




class Get (r :: [(Symbol,*)]) (k :: Symbol) a | r k -> a where
    -- | Get a field of a record.
    access :: Record r -> N k -> a

instance Get ('(k,a) ': xs) k a where
    access (a ::: _) _ = a

instance Get xs k a => Get ('(k1,a1) ': xs) k a where
    access (_ ::: a) f = access a f

class Modify (r0 :: [(Symbol,*)]) (k :: Symbol) a b (r1 :: [(Symbol,*)]) | r0 k a b -> r1 where
    -- | Modify a record's field, with the ability to change its type.
    modify :: N k -> (a -> b) -> Record r0 -> Record r1

instance Modify ('(k, a) ': xs) k a b ('(k, b) ': xs) where
    modify _ f (a ::: xs) = f a ::: xs

instance Modify xs k a b ys => Modify ('(k1,a1) ': xs) k a b ('(k1,a1) ': ys) where
    modify l f (a ::: xs) = a ::: modify l f xs


class Set (r0 :: [(Symbol,*)]) k a b (r1 :: [(Symbol,*)]) | r0 k -> a where
    -- | Write a record's field, with the ability to change its type.
    write :: N k -> b -> Record r0 -> Record r1

instance Set ('(k,a) ': xs) k a b ('(k,b) ': xs ) where
    write _ x (_ ::: xs) = x ::: xs

instance Set xs k a b ys => Set ('(k1,a1) ': xs) k a b ('(k1,a1) ': ys) where
    write l x (y ::: ys) = y ::: write l x ys

-- | Modify a field, without the ability to change its type.
alter :: Modify r k a a r => N k -> (a -> a) -> Record r -> Record r
alter = modify

-- | Set a field, without the ability to change its type
update :: Set r k a a r => N k -> a -> Record r -> Record r
update = write

class Append xs ys where
    -- | Create a new record by appending two, useful for inheritence / subtyping.
    (+++) :: Record xs -> Record ys -> Record (xs ++ ys)
infixr 2 +++

instance Append '[] ys where
    _ +++ ys = ys

instance Append xs ys => Append (x ': xs) ys where
    (x ::: xs) +++ ys = x ::: (xs +++ ys)


-- | Use (:+) as 'cons' for the record fields.
type x :+ xs = x ': xs
infixr 4 :+


type family (++) (x :: [a]) (y :: [a]) :: [a]
type instance '[]       ++ xs = xs
type instance (x ': xs) ++ ys = x ': (xs ++ ys)



