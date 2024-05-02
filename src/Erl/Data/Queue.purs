module Erl.Data.Queue
  ( fromFoldable
  , module ReExport
  )
  where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Erl.Data.Queue.Types (Queue, NonEmptyQueue(..), empty, singleton, put, putFront, get, peek, toList, isEmpty, reverse, drop, length, putBounded) as ReExport
import Erl.Data.Queue.Types as Queue

fromFoldable :: forall f a. Foldable f => f a -> Queue.Queue a
fromFoldable = foldl (flip Queue.put) Queue.empty
