module Test.Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Semigroup.Traversable (traverse1)
import Data.Traversable (sequence, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Data.List as List
import Erl.Data.Queue (NonEmptyQueue(..), Queue)
import Erl.Data.Queue as Queue
import Test.Assert (assertEqual')

main :: Effect Unit
main = do
    let q123 = Queue.put 3 $ Queue.put 2 $ Queue.put 1 $ Queue.empty

    assertEqual' "Queue.length" { actual: Queue.length q123, expected: 3 }
    assertEqual' "Queue.singleton / Queue.put" { actual: Queue.singleton 1, expected: Queue.put 1 Queue.empty }
    assertEqual' "Queue.putBounded" { actual: Queue.putBounded 3 3 $ Queue.putBounded 3 2 $ Queue.putBounded 3 1 $ Queue.empty, expected: q123 }
    assertEqual' "Queue.putBounded - bound=current length" { actual: Queue.putBounded 3 42 q123, expected: Array.toUnfoldable [ 2, 3, 42 ] }
    assertEqual' "Queue.putBounded - bound<current length" { actual: Queue.putBounded 2 42 q123, expected: Array.toUnfoldable [ 3, 42 ] }

    assertEqual' "Queue.get" { actual: Queue.get q123, expected: Just { item: 1, queue: Queue.put 3 $ Queue.put 2 $ Queue.empty } }
    assertEqual' "Queue.peek" { actual: Queue.peek q123, expected: Just 1 }

    assertEqual' "Queue.isEmpty (true)" { actual: Queue.isEmpty Queue.empty, expected: true }
    assertEqual' "Queue.isEmpty (false)" { actual: Queue.isEmpty q123, expected: false }

    assertEqual' "Queue.toList" { actual: Queue.toList q123, expected: 1 :  2 :  3 : nil }
    assertEqual' "unfoldable Queue" { actual: (List.toUnfoldable (1: 2 : 3 : nil)) :: Queue _, expected: q123 }
    assertEqual' "unfoldable1 Queue" { actual: (NEA.toUnfoldable1 (NEA.cons' 1 [2, 3])) :: Queue _, expected: q123 }

    assertEqual' "foldable Queue" { expected: ( 1 :  2 :  3 : nil), actual: List.fromFoldable q123  }

    assertEqual' "traversible Queue" { expected: Just q123, actual: traverse Just q123 }
    assertEqual' "traversible/sequence Queue" { expected: Just q123, actual: sequence $ Queue.put (Just 3) $ Queue.put (Just 2) $ Queue.put (Just 1) $ Queue.empty }
    assertEqual' "functor Queue" { expected: q123, actual: (_ + 1) <$> (Queue.put 2 $ Queue.put 1 $ Queue.put 0 $ Queue.empty) }

    assertEqual' "Queue.fromFoldable" { expected: q123, actual: Queue.fromFoldable [ 1, 2, 3] }
    assertEqual' "traversibleWithIndex Queue" { expected: Just (Queue.fromFoldable [{ i: 0, a: 1 }, { i: 1, a: 2 }, { i: 2, a: 3 } ] ), actual: traverseWithIndex (\i a -> Just { i, a }) q123 }
    assertEqual' "foldableWithIndex Queue" { expected: Just q123, actual: traverse Just q123 }

    assertEqual' "filterable/mapMaybe" { expected: Queue.fromFoldable [ 1, 3 ], actual: filterMap (\a -> if a `mod` 2 == 0 then Nothing else Just a) q123 }

    assertEqual' "traversible1 NonEmptyQueue" { expected: Just (NonEmptyQueue (4 :| q123)), actual: traverse1 Just (NonEmptyQueue (4 :| q123)) }

    pure unit
    