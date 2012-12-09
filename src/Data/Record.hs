-- | Type safe labelled records implemented using GADTs, type literals, and other fun stuff.
-- Functions that use TH are for convenience only -- they are only there to save you from typing a few extra characters
--
-- Extensions you need to use this module are: TypeOperators, DataKinds
--

{-# LANGUAGE UndecidableInstances, OverlappingInstances, TypeFamilies, RecordWildCards, DataKinds, TypeOperators, FunctionalDependencies, GADTs, KindSignatures, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, PolyKinds, EmptyDataDecls #-}
module Data.Record 
    ( n
    , N
    , key
    , set
    , get
    , type (++)
    , Record(..)
    , ($=)
    , (!)
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

-- | More convenience; @ [key|k|] == n :: N k @
key :: QuasiQuoter
key = QuasiQuoter {..}
  where 
    quoteExp  s = [|undefined :: N $(clean s)|]
    quotePat  s = [p|_|] `sigP`[t|N $(clean s)|]
    quoteType s = [t|N $(clean s)|]
    quoteDec    = undefined

clean :: String -> TypeQ
clean = litT . strTyLit . unwords . words

-- | Convenience function making it nicer to update a field of a record.
-- Instead of @ (n :: N k) $= a @ or @ [key|k|] $= a @, you can use @ [set|k|] a @
set :: QuasiQuoter
set = QuasiQuoter{..}
  where
    quoteExp s = [|($=) (undefined :: N $(clean s))|]
    quotePat   = undefined
    quoteType  = undefined
    quoteDec   = undefined

-- | Convenience function making it nicer to get a field of a record.
-- Instead of @ r ! (n :: N k) @, or @ r ! [key|k|] @, you can use @ [get|k|] r @
get :: QuasiQuoter
get = QuasiQuoter{..} 
  where
    quoteExp s = [|flip (!) (undefined :: N $(clean s))|]
    quotePat   = undefined
    quoteType  = undefined
    quoteDec   = undefined


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

instance (Show a, Show (Record xs)) => Show (Record ( '(k, a) ': xs)) where
    show (x ::: xs) = show x ++ " ::: " ++ show xs

instance Eq (Record '[]) where
    _ == _ = True

instance (Eq a, Eq (Record xs)) => Eq (Record ('(k, a) ': xs)) where
    (x ::: xs) == (y ::: ys) = x == y && xs == ys

instance Ord (Record '[]) where
    compare _ _ = EQ

instance (Ord a, Ord (Record xs)) => Ord (Record ('(k, a) ': xs)) where
    compare (x ::: xs) (y ::: ys) = compare (compare x y) (compare xs ys)




class Get (r :: [(Symbol, *)]) (k :: Symbol) a | r k -> a where
    -- | Get a field of a record.
    (!) :: Record r -> N k -> a

-- Success on the first element of a single element record
instance Get '[ '(k, a) ] k a where
    (a ::: _) ! _ = a

-- Success on the first element of a record with 1 or more elements
instance Get ('(k, a) ': xs) k a where
    (a ::: _) ! _ = a

-- Fail the first element, try the rest
instance Get xs k a => Get ('(k1, a1) ': xs) k a where
    (_ ::: a) ! f = a ! f




class Update (r :: [(Symbol, *)]) (k :: Symbol) a | r k -> a where
    -- | Update a field of a record.
    ($=) :: N k -> a -> Record r -> Record r

-- Succeed on the first try
instance Update ( '(k, a) ': xs) k a where
    (_ $= x) (_ ::: b) = x ::: b

-- Fail and try the rest of the record
instance Update xs k a => Update ( '(k1,a1) ': xs) k a where
    (f $= x) (a ::: b) = a ::: (f $= x) b




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



