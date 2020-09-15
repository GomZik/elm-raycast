module Mem exposing (..)

import Dict exposing ( Dict )

type Mem k v = Mem ( k -> v ) ( Dict k v )


lazyWithDefault : ( () -> v ) -> Maybe v -> v
lazyWithDefault fn m =
  case m of
    Just v -> v
    Nothing -> fn ()


empty : ( comparable -> v ) -> Mem comparable v
empty fn =
  Mem fn Dict.empty


get : comparable -> Mem comparable v -> ( v, Mem comparable v )
get k ( Mem fn d ) =
  let
    v = Dict.get k d
      |> lazyWithDefault ( \_ -> fn k )
  in
    (v, Mem fn ( Dict.insert k v d ) )


fill : List comparable -> Mem comparable v -> Mem comparable v
fill vals mem =
  case vals of
    [] -> mem
    x :: xs -> mem
      |> get x
      |> Tuple.second
      |> fill xs
