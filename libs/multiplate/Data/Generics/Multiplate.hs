{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Suppose we are given mutually recursive data types @A@, @B@, and @C@.
-- Here are some definitions of terms.
--
-- [@child@] A maximal subexpression of @A@, @B@, or @C@.
-- A child does not necessarily have to have the same type as the parent.
-- @A@ might have some children of type @B@ and other children of type @C@ or even @A@.
--
-- [@children@] A list of all children.
-- In particular children are ordered from left to right.
--
-- [@descendant@] Any subexpression of of @A@, @B@, or @C@.
-- Specifically a descendant of an expression is either the expression itself or a descendant of one of its children.
--
-- [@family@] A list of all descendant.
-- The order is a context dependent.
-- 'preorderFold' uses preorder, while 'postorderFold' and 'mapFamilyM' uses postorder.
--
-- [@plate@] A plate is a record parametrized by a functor @f@ with one field of type
-- @A -> f A@ for each type belonging to the mutually recursive set of types.  For example,
-- a plate for @A@, @B@, and @C@ would look like
--
-- @
-- data ABCPlate f = ABCPlate
--                 { fieldA :: A -> f A
--                 , fieldB :: B -> f B
--                 , fieldC :: C -> f C
--                 }
-- @
--
-- Although this above is the original motivation behind multiplate,but you can make
-- any structure you want into a 'Multiplate' as long as you satisfy the two multiplate laws listed
-- below.
--
-- The names of the functions in this module are based on Sebastian Fischer's Refactoring Uniplate:
-- <http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html>
module Data.Generics.Multiplate where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid

-- | A plate over @f@ consists of several fields of type @A -> f A@ for various @A@s.
-- 'Projector' is the type of the projection functions of plates.
type Projector p a = forall f. p f -> a -> f a

-- | A 'Multiplate' is a constructor of kind @(* -> *) -> *@ operating on 'Applicative' functors
-- having functions 'multiplate' and 'mkPlate' that satisfy the following two laws:
--
-- (1) @
-- 'multiplate' 'purePlate' = 'purePlate'
--   where
--     'purePlate' = 'mkPlate' (\\_ -> 'pure')
-- @
--
-- (2) @
-- 'multiplate' ('composePlate' p1 p2) = 'composePlate' ('multiplate' p1) ('multiplate' p2)
--   where
--     'composePlate' p1 p2 = 'mkPlate' (\\proj a -> ('Compose' (proj p1 ``fmap`` proj p2 a)))
-- @
--
-- Note: By parametricity, it suffices for (1) to prove
--
-- @
-- 'multiplate' ('mkPlate' (\\_ -> 'Identity')) = 'mkPlate' (\\_ -> 'Identity')
-- @
class Multiplate p where
  -- | This is the heart of the Multiplate library.  Given a plate of functions over some
  -- applicative functor @f@, create a new plate that applies these functions to the children
  -- of each data type in the plate.
  --
  -- This process essentially defines the semantics what the children of these data types are.
  -- They don't have to literally be the syntactic children.  For example, if a language supports
  -- quoted syntax, that quoted syntax behaves more like a literal than as a sub-expression.
  -- Therefore, although quoted expressions may syntactically be subexpressions, the user may
  -- chose to implement 'multiplate' so that they are not semantically considered subexpressions.
  multiplate :: (Applicative f) => p f -> p f

  -- | Given a generic builder creating an @a -> f a@, use the builder to construct each field
  -- of the plate @p f@.  The builder may need a little help to construct a field of type
  -- @a -> f a@, so to help out the builder pass it the projection function for the field
  -- being built.
  --
  -- e.g. Given a plate of type
  --
  -- @
  -- data ABCPlate f = ABCPlate {
  --                 { fieldA :: A -> f B
  --                 , fieldB :: B -> f B
  --                 , fieldC :: C -> f C
  --                 }
  -- @
  --
  -- the instance of 'mkPlate' for @ABCPlate@ should be
  --
  -- @
  --  'mkPlate' builder = ABCPlate (builder fieldA) (builder fieldB) (builder fieldC)
  -- @
  mkPlate :: (forall a. Projector p a -> (a -> f a)) -> p f

-- | Given a natural transformation between two functors, @f@ and @g@, and a plate over
-- @f@, compose the natural transformation with each field of the plate.
applyNaturalTransform :: forall p f g. (Multiplate p) => (forall a. f a -> g a) -> p f -> p g
applyNaturalTransform eta p = mkPlate build
  where
    build :: Projector p a -> a -> g a
    build proj = (eta . proj p)

-- | Given an 'Applicative' @f@, 'purePlate' builds a plate
--  over @f@ whose fields are all 'pure'.
--
--  Generally 'purePlate' is used as the base of a record update. One constructs
--  the expression
--
--  @
--  'purePlate' { /fieldOfInterest/ = \\a -> case a of
--              | /constructorOfInterest/ -> /expr/
--              | _                     -> 'pure' a
--            }
--  @
--
--  and this is a typical parameter that is passed to most functions in this library.
purePlate :: (Multiplate p, Applicative f) => p f
purePlate = mkPlate (\_ -> pure)

-- | Given an 'Alternative' @f@, 'emptyPlate' builds a plate
--  over @f@ whose fields are all @'const' 'empty'@.
--
--  Generally 'emptyPlate' is used as the base of a record update. One constructs
--  the expression
--
--  @
--  'emptyPlate' { /fieldOfInterest/ = \\a -> case a of
--               | /constructorOfInterest/ -> /expr/
--               | _                     -> 'empty'
--             }
--  @
--
--  and this is a typical parameter that is passed to 'evalFamily' and 'evalFamilyM'.
emptyPlate :: (Multiplate p, Alternative f) => p f
emptyPlate = mkPlate (\_ _ -> empty)

-- | Given two plates over a monad @m@, the fields of the plate can be
-- Kleisli composed ('<=<') fieldwise.
kleisliComposePlate :: forall p m. (Multiplate p, Monad m) => p m -> p m -> p m
kleisliComposePlate f1 f2 = mkPlate build
  where
    build :: Projector p a -> a -> m a
    build proj = (proj f1 <=< proj f2)

-- | Given two plates, they can be composed fieldwise yielding the composite functor.
composePlate :: forall p f g. (Multiplate p, Functor g) => p f -> p g -> p (Compose g f)
composePlate f1 f2 = mkPlate build
  where
    build :: Projector p a -> a -> Compose g f a
    build proj a = (Compose (proj f1 `fmap` proj f2 a))

-- | Given two plates with one over the 'Identity' functor, the two plates
-- can be composed fieldwise.
composePlateRightId :: forall p f. (Multiplate p) => p f -> p Identity -> p f
composePlateRightId f1 f2 = mkPlate build
  where
    build :: Projector p a -> a -> f a
    build proj = (proj f1 . traverseFor proj f2)

-- | Given two plates with one over the 'Identity' functor, the two plates
-- can be composed fieldwise.
composePlateLeftId :: forall p f. (Multiplate p, Functor f) => p Identity -> p f -> p f
composePlateLeftId f1 f2 = mkPlate build
  where
    build :: Projector p a -> a -> f a
    build proj a = (traverseFor proj f1 `fmap` proj f2 a)

-- | Given two plates with one over the @'Constant' o@ applicative functor for a 'Monoid' @o@,
-- each field of the plate can be pointwise appended with 'mappend'.
appendPlate :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o) -> p (Constant o)
appendPlate f1 f2 = mkPlate build
  where
    build :: Projector p a -> a -> Constant o a
    -- both <* and *> are the same for the Constant applicative functor
    build proj a = (proj f1 a <* proj f2 a)

-- | Given a plate whose fields all return a 'Monoid' @o@,
-- 'mChildren' produces a plate that returns the 'mconcat'
-- of all the children of the input.
mChildren :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o)
mChildren = multiplate

-- | Given a plate whose fields all return a 'Data.Monoid.Monoid' @o@,
-- 'preorderFold' produces a plate that returns the 'Data.Monoid.mconcat'
-- of the family of the input. The input itself produces the leftmost element
-- of the concatenation, then this is followed by the family of the first child, then
-- it is followed by the family of the second child, and so forth.
preorderFold :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o)
preorderFold f = f `appendPlate` multiplate (preorderFold f)

-- | Given a plate whose fields all return a 'Data.Monoid.Monoid' @o@,
-- 'preorderFold' produces a plate that returns the 'Data.Monoid.mconcat'
-- of the family of the input. The concatenation sequence begins with
-- the family of the first child, then
-- it is followed by the family of the second child, and so forth until finally
-- the input itself produces the rightmost element of the concatenation.
postorderFold :: forall p o. (Multiplate p, Monoid o) => p (Constant o) -> p (Constant o)
postorderFold f = multiplate (postorderFold f) `appendPlate` f

-- | Given a plate whose fields transform each type, 'mapChildren'
-- returns a plate whose fields transform the children of the input.
mapChildren :: (Multiplate p) => p Identity -> p Identity
mapChildren = multiplate

-- | Given a plate whose fields transform each type, 'mapFamily'
-- returns a plate whose fields transform the family of the input.
-- The traversal proceeds bottom up, first transforming the families of
-- the children, before finally transforming the value itself.
mapFamily :: (Multiplate p) => p Identity -> p Identity
mapFamily = mapFamilyM

-- | Given a plate whose fields transform each type, 'mapChildrenM'
-- returns a plate whose fields transform the children of the input.
-- The processing is sequenced from the first child to the last child.
mapChildrenM :: (Multiplate p, Applicative m, Monad m) => p m -> p m
mapChildrenM = multiplate

-- | Given a plate whose fields transform each type, 'mapFamilyM'
-- returns a plate whose fields transform the family of the input.
-- The sequencing is done in a depth-first postorder traversal.
mapFamilyM :: (Multiplate p, Applicative m, Monad m) => p m -> p m
mapFamilyM f = f `kleisliComposePlate` (multiplate (mapFamilyM f))

-- | Given a plate whose fields maybe transforms each type, 'evalFamily'
-- returns a plate whose fields exhaustively transform the family of the input.
-- The traversal proceeds bottom up, first transforming the families of
-- the children. If a transformation succeeds then the result is re-'evalFamily'ed.
--
-- A post-condition is that the input transform returns 'Nothing' on all family members
-- of the output, or more formally
--
-- @
-- 'preorderFold' ('applyNaturalTransform' t f) ``composePlate`` ('evalFamily' f) &#x2291; 'purePlate'
--  where
--   t :: forall a. 'Maybe' a -> 'Constant' 'All' a
--   t = 'Constant' '.' 'All' '.' 'isNothing'
-- @
evalFamily :: (Multiplate p) => p Maybe -> p Identity
evalFamily f = evalFamilyM (applyNaturalTransform (MaybeT . Identity) f)

-- | Given a plate whose fields maybe transforms each type, 'evalFamilyM'
-- returns a plate whose fields exhaustively transform the family of the input.
-- The sequencing is done in a depth-first postorder traversal, but
-- if a transformation succeeds then the result is re-'evalFamilyM'ed.
evalFamilyM :: forall p m. (Multiplate p, Applicative m, Monad m) => p (MaybeT m) -> p m
evalFamilyM f = go
  where
    go = mapFamilyM (mkPlate eval)
    eval :: Projector p a -> a -> m a
    eval proj a = maybe (return a) (proj go) =<< (runMaybeT (proj f a))

-- | Given a plate used for 'evalFamily', replace returning 'Nothing'
-- with returning the input.  This transforms plates suitable for 'evalFamily'
-- into plates suitable form 'mapFamily'.
always :: (Multiplate p) => p Maybe -> p Identity
always f = alwaysM (applyNaturalTransform (MaybeT . Identity) f)

-- | Given a plate used for 'evalFamilyM', replace returning 'Nothing'
-- with returning the input.  This transforms plates suitable for 'evalFamilyM'
-- into plates suitable form 'mapFamilyM'.
alwaysM :: forall p f. (Multiplate p, Functor f) => p (MaybeT f) -> p f
alwaysM f = mkPlate build
  where
    build :: Projector p a -> a -> f a
    build proj a = (fromMaybe a) `fmap` (runMaybeT (proj f a))

-- | Given a projection function for a plate over the 'Identity' functor,
-- upgrade the projection function to strip off the wrapper.
traverseFor :: (Multiplate p) => Projector p a -> p Identity -> a -> a
traverseFor proj f = runIdentity . proj f

-- | Instantiate a projection function at a monad.
traverseMFor :: (Multiplate p, Monad m) => Projector p a -> p m -> a -> m a
traverseMFor proj f = proj f

-- | Given a projection function for a plate over the @'Constant' o@ functor,
-- upgrade the projection function to strip off the wrapper.
foldFor :: (Multiplate p) => Projector p a -> p (Constant o) -> a -> o
foldFor proj f = getConstant . proj f

-- | Given a projection function for a plate over the @'Constant' o@ functor,
-- and a continuation for @o@, upgrade  the projection function to strip off the wrapper
-- and run the continuation.
--
-- Typically the continuation simply strips off a wrapper for @o@.
unwrapFor :: (Multiplate p) => (o -> b) -> Projector p a -> p (Constant o) -> a -> b
unwrapFor unwrapper proj f = unwrapper . foldFor proj f

-- | Given a projection function for a plate over the @'Constant' ('Sum' n)@ functor,
-- upgrade the projection function to strip off the wrappers.
sumFor :: (Multiplate p) => Projector p a -> p (Constant (Sum n)) -> a -> n
sumFor = unwrapFor getSum

-- | Given a projection function for a plate over the @'Constant' ('Product' n)@ functor,
-- upgrade the projection function to strip off the wrappers.
productFor :: (Multiplate p) => Projector p a -> p (Constant (Product n)) -> a -> n
productFor = unwrapFor getProduct

-- | Given a projection function for a plate over the @'Constant' 'All'@ functor,
-- upgrade the projection function to strip off the wrappers.
allFor :: (Multiplate p) => Projector p a -> p (Constant All) -> a -> Bool
allFor = unwrapFor getAll

-- | Given a projection function for a plate over the @'Constant' 'Any'@ functor,
-- upgrade the projection function to strip off the wrappers.
anyFor :: (Multiplate p) => Projector p a -> p (Constant Any) -> a -> Bool
anyFor = unwrapFor getAny

-- | Given a projection function for a plate over the @'Constant' ('First' n)@ functor,
-- upgrade the projection function to strip off the wrappers.
firstFor :: (Multiplate p) => Projector p a -> p (Constant (First b)) -> a -> Maybe b
firstFor = unwrapFor getFirst

-- | Given a projection function for a plate over the @'Constant' ('Last' n)@ functor,
-- upgrade the projection function to strip off the wrappers.
lastFor :: (Multiplate p) => Projector p a -> p (Constant (Last b)) -> a -> Maybe b
lastFor = unwrapFor getLast
