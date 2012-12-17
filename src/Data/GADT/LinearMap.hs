{-# LANGUAGE GADTs, KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Data.GADT.LinearMap
-- License     :  BSD3
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Linear transformation as category & arrow.
-- See <http://conal.net/blog/posts/reimagining-matrices/>
-- for motivation and derivations.
----------------------------------------------------------------------

module Data.GADT.LinearMap ((:-*)(..),apply,fstL,sndL) where

import Prelude hiding (id,(.))

import Data.VectorSpace

import Control.ConstraintKinds.Category

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

type a :* b = (a,b)

{--------------------------------------------------------------------
    General linear transformations
--------------------------------------------------------------------}

type VS a = (InnerSpace a, HasZero a, HasScale a, Num (Scalar a))

type a :~ b = Scalar a ~ Scalar b

type CV b a = (VS b, b :~ a)  -- compatible vector space

type VS2 a b     = (VS a     , CV b a)
type VS3 a b c   = (VS2 a b  , CV c a)
type VS4 a b c d = (VS3 a b c, CV d a)

class VectorSpace v => HasScale v where
  scale :: Scalar v -> v :-* v

idL :: (HasScale v, Num (Scalar v)) => v :-* v
idL = scale 1

instance VS2 a b => HasScale (a :* b) where
  scale s = scale s *** scale s

class HasZero z where zeroL :: CV a z => a :-* z

instance VS2 a b => HasZero (a :* b) where
  zeroL = zeroL &&& zeroL

--     Variable occurs more often in a constraint than in the instance head
--       in the constraint: VS a
--     (Use -XUndecidableInstances to permit this)
--     In the instance declaration for `HasScale (a, b)'

#define ScalarType(t) \
  instance HasZero  (t) where { zeroL = Dot zeroV } ; \
  instance HasScale (t) where scale = Dot

ScalarType(Int)
ScalarType(Integer)
ScalarType(Float)
ScalarType(Double)

infix  7 :&&
infixr 1 :-*

-- | Linear transformation
data (:-*) :: * -> * -> * where
  Dot   :: InnerSpace b =>
           b -> b :-* Scalar b
  (:&&) :: VS3 a c d =>
           a :-* c -> a :-* d -> a :-* c :* d

-- | Semantic function: sample a linear transformation
apply :: a :-* b -> a -> b
apply (Dot b)   = dot b
apply (f :&& g) = apply f &&& apply g

dot :: InnerSpace b => b -> b -> Scalar b
dot = (<.>)

instance Category (:-*) where
  type CategoryConstraint (:-*) a b = VS2 a b
  id = idL
  (f :&& g) . h = f . h &&& g . h
  Dot s  . Dot b     = Dot (s *^ b)          -- s must be scalar
  Dot ab . (f :&& g) = Dot a . f ^+^ Dot b . g where (a,b) = ab

-- The GHC 7.4.1 type-checker balks at the Dot (a,b) pattern, so I used a where.


instance CategoryProduct (:-*) where
  type CP12 (:-*) a   c d = VS3 a   c d
  type CP22 (:-*) a b c d = VS4 a b c d
  (&&&) = (:&&)
  f *** g = compFst f &&& compSnd g
  -- Equivalently,
  -- f *** g = f . fstL &&& g . sndL

instance VS2 a b => AdditiveGroup (a :-* b) where
  zeroV   = zeroL
  negateV = (scale (-1) .)
  Dot b     ^+^ Dot c     = Dot (b ^+^ c)
  (f :&& g) ^+^ (h :&& k) = (f ^+^ h) &&& (g ^+^ k)
  _         ^+^ _         = error "(^+^) for a :-* b: unexpected combination"

-- The last case cannot arise unless pairs are scalars.

instance VS2 a b => VectorSpace (a :-* b) where
  type Scalar (a :-* b) = Scalar b
  s *^ Dot b     = Dot (s *^ b)
  s *^ (f :&& g) = s *^ f &&& s *^ g

-- InnerSpace instance?

-- | @apply (compFst f) == apply f . fst@
compFst :: VS3 a b c => a :-* c -> a :* b :-* c
compFst (Dot a)   = Dot (a,zeroV)
compFst (f :&& g) = compFst f &&& compFst g

-- dot a . fst = dot (a,0)
--
-- (f &&& g) . fst = f . fst &&& g . fst

-- | @apply (compSnd f) == apply f . snd@
compSnd :: VS3 a b c => b :-* c -> a :* b :-* c
compSnd (Dot b)   = Dot (zeroV,b)
compSnd (f :&& g) = compSnd f &&& compSnd g



fstL :: VS2 a b => a :* b :-* a
fstL = compFst id

sndL :: VS2 a b => a :* b :-* b
sndL = compSnd id
