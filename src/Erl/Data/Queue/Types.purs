module Erl.Data.Queue.Types
       ( Queue
       , NonEmptyQueue(..)
       , empty
       , singleton
       , put
       , get
       , peek
       , isEmpty
       , reverse
       , toList
       ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Control.Comonad (class Comonad, class Extend)
import Control.MonadPlus (class MonadPlus)
import Data.Compactable (class Compactable, separateDefault)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Filterable (class Filterable)
import Data.Foldable (class Foldable, foldMapDefaultR, foldl, foldr, intercalate)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldMapWithIndexDefaultR, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.List.NonEmpty (traverse1)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Ord (class Ord1, compare1)
import Data.Semigroup.Foldable (class Foldable1)
import Data.Semigroup.Traversable (class Traversable1)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, class Unfoldable1)
import Data.Witherable (class Witherable, wiltDefault, witherDefault)
import Erl.Data.List (List)

foreign import data Queue :: Type -> Type

foreign import empty :: forall a. Queue a

foreign import singleton :: forall a. a -> Queue a

foreign import put :: forall a. a -> Queue a -> Queue a

get :: forall a. Queue a -> Maybe { item :: a, queue :: Queue a }
get = getImpl Just Nothing
foreign import getImpl :: forall a. (a -> Maybe a) -> Maybe a -> Queue a -> Maybe { item :: a, queue :: Queue a }

foreign import drop :: forall a. Queue a -> Queue a

peek :: forall a. Queue a -> Maybe a
peek = peekImpl Just Nothing
foreign import peekImpl :: forall a. (a -> Maybe a) -> Maybe a -> Queue a -> Maybe a

foreign import isEmpty :: forall a. Queue a -> Boolean

foreign import reverse :: forall a. Queue a -> Queue a

foreign import toList :: forall a. Queue a -> List a

instance showQueue :: Show a => Show (Queue a) where
  show queue | isEmpty queue = "nil"
  show queue = "(" <> intercalate " : " (show <$> queue) <> " : nil)"

instance eqQueue :: Eq a => Eq (Queue a) where
  eq = eq1

instance eq1Queue :: Eq1 Queue where
  eq1 queueLhs queueRhs = go queueLhs queueRhs
    where
      go queueLhs' queueRhs' =
        case get queueLhs', get queueRhs' of
          Nothing, Nothing -> true
          Just { item: x, queue: queueLhs'' }, Just { item: y, queue: queueRhs'' } | x == y -> go queueLhs'' queueRhs''
          _, _ -> false

instance ordQueue :: Ord a => Ord (Queue a) where
  compare = compare1

-- Adapted from https://hackage.haskell.org/package/base-4.4.1.0/docs/src/GHC-Classes.html
instance ord1Queue :: Ord1 Queue where
  compare1 queueLhs queueRhs =
      case get queueLhs, get queueRhs of
         Nothing, Nothing -> EQ
         Nothing, _       -> LT
         _, Nothing       -> GT
         Just { item: x, queue: queueLhs'' }, Just { item: y, queue: queueRhs'' } ->
           case compare x y of
             EQ -> compare1 queueLhs'' queueRhs''
             other -> other

instance semigroupQueue :: Semigroup (Queue a) where
  append = appendImpl

foreign import appendImpl :: forall a. Queue a -> Queue a -> Queue a

instance monoidQueue :: Monoid (Queue a) where
  mempty = empty

instance functorQueue :: Functor Queue where
  map = mapImpl

instance foldableQueue :: Foldable Queue where
  foldr = foldrImpl
  foldl = foldlImpl
  foldMap = foldMapDefaultR

foreign import mapImpl :: forall a b. (a -> b) -> Queue a -> Queue b

foreign import foldrImpl :: forall a b. (a -> b -> b) -> b -> Queue a -> b

foreign import foldlImpl :: forall a b. (b -> a -> b) -> b -> Queue a -> b

instance unfoldable1Queue :: Unfoldable1 Queue where
  unfoldr1 f b = go b empty
    where
      go source memo = case f source of
        Tuple one Nothing -> foldl (flip put) empty (put one memo)
        Tuple one (Just rest) -> go rest (put one memo)

instance unfoldableQueue :: Unfoldable Queue where
  unfoldr f b = go b empty
    where
      go source memo = case f source of
        Nothing -> foldl (flip put) empty memo
        Just (Tuple one rest) -> go rest (put one memo)

instance traversableQueue :: Traversable Queue where
  traverse f queue =
    case get queue of
      Nothing -> pure empty
      Just { item, queue: newQueue } -> put <$> f item <*> traverse f newQueue
  sequence queue =
    case get queue of
      Nothing -> pure empty
      Just { item, queue: newQueue } -> put <$> item <*> sequence newQueue

instance traversableWithIndexQueue :: TraversableWithIndex Int Queue where
  traverseWithIndex f initialQueue =
    traverseWithIndexImpl initialQueue 0
    where
    traverseWithIndexImpl queue i =
      case get queue of
        Nothing -> pure empty
        Just { item, queue: newQueue } -> put <$> f i item <*> traverseWithIndexImpl newQueue (i+1)

instance foldableWithIndexQueue :: FoldableWithIndex Int Queue where
  foldrWithIndex f z lst = foldr (\(Tuple i x) y -> f i x y) z $ mapWithIndex Tuple lst
  foldlWithIndex f z lst = foldl (\y (Tuple i x) -> f i y x) z $ mapWithIndex Tuple lst
  foldMapWithIndex f = foldMapWithIndexDefaultR f

instance functorWithIndexQueue :: FunctorWithIndex Int Queue where
  mapWithIndex f lst = foldl (flip put) empty $ go 0 lst empty
    where
    go n l acc = case get l of
      Nothing -> acc
      Just { item: x, queue: xs } -> go (n+1) xs $ put (f n x) acc

instance applyQueue :: Apply Queue where
  apply queue xs =
    case get queue of
      Nothing -> empty
      Just { item: f, queue: fs } -> (f <$> xs) <> (fs <*> xs)

instance applicativeQueue :: Applicative Queue where
  pure a = put a empty

instance bindQueue :: Bind Queue where
  bind l f = case get l of
    Nothing -> empty
    Just { item: x, queue: xs } -> f x <> bind xs f

instance monadQueue :: Monad Queue

instance altQueue :: Alt Queue where
  alt = append

instance plusQueue :: Plus Queue where
  empty = empty

instance alternativeQueue :: Alternative Queue

instance monadPlusQueue :: MonadPlus Queue

-- | Apply a function to each element in a queue, keeping only the results which
-- | contain a value.
-- |
-- | Running time: `O(n)`
mapMaybe :: forall a b. (a -> Maybe b) -> Queue a -> Queue b
mapMaybe f = go empty
  where
  go acc l = case get l of
    Nothing -> foldl (flip put) empty acc
    Just { item: x, queue: xs } ->
      case f x of
        Nothing -> go acc xs
        Just y -> go (put y acc) xs

-- | Filter a queue, keeping the elements which satisfy a predicate function.
-- |
-- | Running time: `O(n)`
foreign import filter :: forall a. (a -> Boolean) -> Queue a -> Queue a

instance compactableQueue :: Compactable Queue where
  compact = mapMaybe identity
  separate xs = separateDefault xs

instance filterableQueue :: Filterable Queue where
  partitionMap :: forall a l r. (a -> Either l r) -> Queue a -> { left :: Queue l, right :: Queue r }
  partitionMap p xs = foldr select { left: empty, right: empty } xs
      where
        select x { left, right } = case p x of
                                     Left l -> { left: put l left, right }
                                     Right r -> { left, right: put r right }

  partition :: forall a. (a -> Boolean) -> Queue a -> { no :: Queue a, yes :: Queue a }
  partition p xs = foldr select { no: empty, yes: empty } xs
      where
        -- select :: (a -> Boolean) -> a -> { no :: Queue a, yes :: Queue a } -> { no :: Queue a, yes :: Queue a }
        select x { no, yes } = if p x
                                 then { no, yes: put x yes }
                                 else { no: put x no, yes }


  filterMap :: forall a b. (a -> Maybe b) -> Queue a -> Queue b
  filterMap = mapMaybe

  filter :: forall a. (a -> Boolean) -> Queue a -> Queue a
  filter = filter

instance witherableQueue :: Witherable Queue where
  wilt = wiltDefault
  wither = witherDefault

--------------------------------------------------------------------------------
-- NonEmpty
--------------------------------------------------------------------------------

newtype NonEmptyQueue a = NonEmptyQueue (NonEmpty Queue a)

toQueue :: NonEmptyQueue ~> Queue
toQueue (NonEmptyQueue (x :| xs)) = put x xs

nelPut :: forall a. a -> NonEmptyQueue a -> NonEmptyQueue a
nelPut a (NonEmptyQueue (b :| bs)) = NonEmptyQueue (a :| put b bs)

derive instance newtypeNonEmptyQueue :: Newtype (NonEmptyQueue a) _

derive newtype instance eqNonEmptyQueue :: Eq a => Eq (NonEmptyQueue a)
derive newtype instance ordNonEmptyQueue :: Ord a => Ord (NonEmptyQueue a)

instance showNonEmptyQueue :: Show a => Show (NonEmptyQueue a) where
  show (NonEmptyQueue nel) = "(NonEmptyQueue " <> show nel <> ")"

derive newtype instance functorNonEmptyQueue :: Functor NonEmptyQueue

instance applyNonEmptyQueue :: Apply NonEmptyQueue where
  apply (NonEmptyQueue (f :| fs)) (NonEmptyQueue (a :| as)) =
    NonEmptyQueue (f a :| (fs <*> put a empty) <> ((put f fs) <*> as))

instance applicativeNonEmptyQueue :: Applicative NonEmptyQueue where
  pure = NonEmptyQueue <<< NE.singleton

instance bindNonEmptyQueue :: Bind NonEmptyQueue where
  bind (NonEmptyQueue (a :| as)) f =
    case f a of
      NonEmptyQueue (b :| bs) ->
        NonEmptyQueue (b :| bs <> bind as (toQueue <<< f))

instance monadNonEmptyQueue :: Monad NonEmptyQueue

instance altNonEmptyQueue :: Alt NonEmptyQueue where
  alt = append

instance extendNonEmptyQueue :: Extend NonEmptyQueue where
  extend f w@(NonEmptyQueue (_ :| as)) =
    NonEmptyQueue (f w :| (foldr go { val: empty, acc: empty } as).val)
    where
    go a { val, acc } = { val: put (f (NonEmptyQueue (a :| acc))) val, acc: put a acc }

instance comonadNonEmptyQueue :: Comonad NonEmptyQueue where
  extract (NonEmptyQueue (a :| _)) = a

instance semigroupNonEmptyQueue :: Semigroup (NonEmptyQueue a) where
  append (NonEmptyQueue (a :| as)) as' =
    NonEmptyQueue (a :| as <> toQueue as')

derive newtype instance foldableNonEmptyQueue :: Foldable NonEmptyQueue

derive newtype instance traversableNonEmptyQueue :: Traversable NonEmptyQueue

derive newtype instance foldable1NonEmptyQueue :: Foldable1 NonEmptyQueue

derive newtype instance unfoldable1NonEmptyQueue :: Unfoldable1 NonEmptyQueue

instance functorWithIndexNonEmptyQueue :: FunctorWithIndex Int NonEmptyQueue where
  mapWithIndex fn (NonEmptyQueue ne) = NonEmptyQueue $ mapWithIndex (fn <<< maybe 0 (add 1)) ne

instance foldableWithIndexNonEmptyQueue :: FoldableWithIndex Int NonEmptyQueue where
  foldMapWithIndex f (NonEmptyQueue ne) = foldMapWithIndex (f <<< maybe 0 (add 1)) ne
  foldlWithIndex f b (NonEmptyQueue ne) = foldlWithIndex (f <<< maybe 0 (add 1)) b ne
  foldrWithIndex f b (NonEmptyQueue ne) = foldrWithIndex (f <<< maybe 0 (add 1)) b ne

instance traversableWithIndexNonEmptyQueue :: TraversableWithIndex Int NonEmptyQueue where
  traverseWithIndex f (NonEmptyQueue ne) = NonEmptyQueue <$> traverseWithIndex (f <<< maybe 0 (add 1)) ne

instance traversable1NonEmptyQueue :: Traversable1 NonEmptyQueue where
  traverse1 f (NonEmptyQueue (a :| as)) =
    foldl (\acc -> lift2 (flip nelPut) acc <<< f) (pure <$> f a) as
      <#> case _ of NonEmptyQueue (x :| xs) → foldl (flip nelPut) (pure x) xs
  sequence1 = traverse1 identity
