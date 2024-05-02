-module(erl_data_queue_types@foreign).
-export([ empty/0
        , length/1
        , singleton/1
        , put/2
        , putFront/2
        , getImpl/3
        , drop/1
        , peekImpl/3
        , isEmpty/1
        , reverse/1
        , toList/1
        , appendImpl/2
        , mapImpl/2
        , foldrImpl/3
        , foldlImpl/3
        , filter/2
        ]).

empty() -> queue:new().

length(Queue) -> queue:len(Queue).

singleton(X) -> queue:in(X, queue:new()).

put(X, Queue) -> queue:in(X, Queue).

putFront(X, Queue) -> queue:in_r(X, Queue).

getImpl(Just, Nothing, Queue) ->
  case queue:out(Queue) of
    {{value, Item}, Q2} ->
      Just(#{item => Item, queue => Q2});
    {empty, _} ->
      Nothing
  end.

drop(Queue) ->
  try queue:drop(Queue) of
    NewQueue -> NewQueue
  catch
    error:empty -> Queue
  end.

peekImpl(Just, Nothing, Queue) ->
  case queue:peek(Queue) of
    empty -> Nothing;
    {value, Item} -> Just(Item)
  end.

isEmpty(Queue) -> queue:is_empty(Queue).

reverse(Queue) -> queue:reverse(Queue).

toList(Queue) -> queue:to_list(Queue).

appendImpl(Queue1, Queue2) -> queue:join(Queue1, Queue2).

mapImpl(Fn, Queue) -> queue:from_list(lists:map(Fn, queue:to_list(Queue))).

foldlImpl(F, I, Queue) -> lists:foldl(fun (X, A) -> (F(A))(X) end, I, queue:to_list(Queue)).
foldrImpl(F, I, Queue) -> lists:foldr(fun (X, A) -> (F(X))(A) end, I, queue:to_list(Queue)).

filter(F, Queue) -> queue:filter(F, Queue).
