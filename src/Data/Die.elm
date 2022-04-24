module Data.Die exposing (..)

import Random exposing (Generator)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Index, Six)
import StaticArray.Length as Length exposing (Length)


type alias Die =
    Index Six


length : Length Six
length =
    Length.six


fromInt : Int -> Die
fromInt =
    Index.fromLessThen length


add : Int -> Die -> Maybe Die
add int die =
    die
        |> Index.toInt
        |> (+) int
        |> Index.fromInt length


random : Generator Die
random =
    Random.int 0 5
        |> Random.map (Index.fromLessThen length)


internalAsString : StaticArray Six String
internalAsString =
    ( "⚀", [ "⚁", "⚂", "⚃", "⚄", "⚅" ] )
        |> StaticArray.fromList length


toString : Die -> String
toString dice =
    internalAsString
        |> StaticArray.get dice
