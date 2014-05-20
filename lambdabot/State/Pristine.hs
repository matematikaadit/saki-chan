{-# LANGUAGE Safe #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module L where
-- TODO: unordered-containters, text, vector (+ lenses thereof)
import Control.Applicative
import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as C
import Control.Exception
import Control.Exception.Lens
import Control.Lens hiding (index, indices)
import qualified Control.Lens as Lens
import qualified Control.Lens
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Error.Lens
import Control.Monad.Fix
import Control.Monad.Identity
-- import Control.Monad.Instances
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.ST.Safe
import Control.Monad.State
import Control.Monad.Writer
-- import Control.Parallel
-- import Control.Parallel.Strategies
-- import Control.Parallel.Strategies.Lens
import Data.Array
import Data.Array.Lens
import Data.Bits
import Data.Bits.Lens
import Data.Bool
import Data.Char
import Data.Complex
import Data.Complex.Lens
import Data.Data
import Data.Data.Lens
import Data.Default
import Data.Dynamic
import Data.Dynamic.Lens
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Function
import Data.Graph
import Data.Int
import Data.Ix
import Data.List
import Data.List.Lens
import Data.List.Split
import Data.List.Split.Lens
import Data.Maybe
import Data.Monoid
-- import Data.Number.BigFloat
import Data.Number.CReal
import Data.Number.Interval
import Data.Number.Natural
import Data.Number.Symbolic hiding (var)
import qualified Data.Number.Symbolic
import qualified Data.Number.Symbolic as Sym
import Data.Ord
import Data.Ratio
import Data.STRef
import Data.Tree
import Data.Tree.Lens
import Data.Tuple
import Data.Typeable
import Data.Typeable.Lens
import Data.Default
-- import Data.Void
-- import Data.Universe
import Data.Word
import Debug.SimpleReflect
import Numeric
-- import Numeric.AD
-- import Numeric.AD.Types
import Numeric.Lens
import ShowFun
import System.Random hiding (split)
import qualified System.Random
import qualified System.Random as R
import Lambdabot.Plugin.Haskell.Eval.Trusted
import Text.PrettyPrint.HughesPJ hiding (empty, first, (<>))
import qualified Text.PrettyPrint.HughesPJ
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.ByteString.Lens
import qualified Data.Foldable as F
import Data.Foldable (Foldable, fold, foldMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Sequence.Lens
import qualified Data.Set as S
import Data.Set.Lens
import qualified Data.Traversable as T
-- import Math.NumberTheory.GCD
-- import Math.NumberTheory.Logarithms
-- import Math.NumberTheory.Moduli
-- import Math.NumberTheory.MoebiusInversion
-- import Math.NumberTheory.Powers
-- import Math.NumberTheory.Primes
-- import Math.NumberTheory.Primes.Factorisation.Certified
-- import Math.NumberTheory.Primes.Testing.Certificates

default ((),Integer,Double)

deriving instance Show a => Show (Identity a)
deriving instance Read a => Read (Identity a)

deriving instance Show (f (Mu f)) => Show (Mu f)
deriving instance Read (f (Mu f)) => Read (Mu f)

infixl 0 `asTypeIn`
asTypeIn :: a -> (a -> b) -> a
x `asTypeIn` _ = x

infixl 0 `asAppliedTo`
asAppliedTo :: (a -> b) -> a -> a -> b
asAppliedTo f _ = f

-- FIXME: should just import Mu from elsewhere and avoid duplicating
-- the instances/definitions
cata :: Functor f => (f a -> a) -> Mu f -> a
cata f = f . fmap (cata f) . out

-- TODO: source from a package? define in terms of lens' Equality?
data Is a b where Refl :: Is a a

{-# LINE 1 "<local>" #-}
