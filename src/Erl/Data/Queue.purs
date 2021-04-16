module Erl.Data.Queue
       ( module Erl.Data.Queue.Types
       )
       where

import Erl.Data.Queue.Types (Queue, NonEmptyQueue(..), empty, singleton, put, get, peek, toList, isEmpty, reverse)
