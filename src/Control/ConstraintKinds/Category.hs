{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-} -- see below
{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Control.ConstraintKinds.Category
-- License     :  BSD3
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Category class with associated constraint
-- Copied & tweaked from Control.Category in base
----------------------------------------------------------------------

module Control.ConstraintKinds.Category
  ( Category(..) -- ,(<<<),(>>>)
  , CategoryConstraint0
  , CategoryProduct(..)
  ) where

-- TODO: explicit exports

import Prelude hiding (id,(.))
import qualified Prelude

import GHC.Prim (Constraint)

infixr 9 .
-- infixr 1 >>>, <<<

-- | A class for categories.
--   id and (.) must form a monoid.
class Category cat where
    type CategoryConstraint cat a b :: Constraint
    -- type CategoryConstraint cat a b = CategoryConstraint0 cat a b
    type CategoryConstraint cat a b = CategoryConstraint0 cat a b
    -- | the identity morphism
    id :: CategoryConstraint cat a a =>
          cat a a
    -- | morphism composition
    (.) :: (CategoryConstraint cat a b, CategoryConstraint cat b c) =>
           cat b c -> cat a b -> cat a c

-- Workaround for problem with empty default associated constraints.
class CategoryConstraint0 (cat :: * -> * -> *) a b
instance CategoryConstraint0 cat a b

type CC cat a b = CategoryConstraint cat a b

{-# RULES
"identity/left"  forall p .
                id . p = p
"identity/right" forall p .
                p . id = p
-- "association"   forall p q r .
--                 (p . q) . r = p . (q . r)
 #-}

instance Category (->) where
    id = Prelude.id
-- #ifndef __HADDOCK__
-- Haddock 1.x cannot parse this:
    (.) = (Prelude..)
-- #endif

{-
-- | Right-to-left composition
(<<<) :: (Category cat, CC cat a, CC cat b, CC cat c) => 
         cat b c -> cat a b -> cat a c
(<<<) = (.)

-- | Left-to-right composition
(>>>) :: (Category cat, CC cat a, CC cat b, CC cat c) => 
         cat a b -> cat b c -> cat a c
(>>>) = flip (<<<)
-}

infixr 3 ***
infixr 3 &&&

class Category cat => CategoryProduct cat where
  type CP12 cat a b c   :: Constraint
  type CP12 cat a b c   =  CC cat a (b,c)
  type CP22 cat a b c d :: Constraint
  type CP22 cat a b c d =  CC cat (a,b) (c,d)
  (***) :: (CC cat a c, CC cat b d, CP22 cat a b c d) =>
           cat a c -> cat b d -> cat (a,b) (c,d)
  (&&&) :: (CC cat a c, CC cat a d, CP12 cat a c d) =>
           cat a c -> cat a d -> cat   a   (c,d)
  first  :: (CC cat a c, CC cat b b, CP22 cat a b c b) =>
            cat a c -> cat (a,b) (c,b)
  first  = (*** id)
  second :: (CC cat b d, CC cat a a, CP22 cat a b a d) =>
            cat b d -> cat (a,b) (a,d)
  second = (id ***)

--     Application is no smaller than the instance head
--       in the type family application: CategoryConstraint cat a (b, c)
--     (Use -XUndecidableInstances to permit this)
--     In the class declaration for `CategoryProduct'
--
--     Application is no smaller than the instance head
--       in the type family application: CategoryConstraint
--                                         cat (a, b) (c, d)
--     (Use -XUndecidableInstances to permit this)
--     In the class declaration for `CategoryProduct'

instance CategoryProduct (->) where
  (f *** g) ~(x,y) = (f x, g y)
  f &&& g = (f *** g) . (\ x -> (x,x))

-- TODO: Perhaps add dup, fst, snd
