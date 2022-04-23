module Data.Dice exposing (..)

import Random exposing (Generator)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Index, Six)
import StaticArray.Length as Length exposing (Length)


type alias Dice =
    Index Six


length : Length Six
length =
    Length.plus1 Length.five


random : Generator Dice
random =
    Random.int 0 5 |> Random.map (Index.fromLessThen length)


internalAsString : StaticArray Six String
internalAsString =
    [ "⚀", "⚁", "⚂", "⚃", "⚄", "⚅" ]
        |> StaticArray.fromList length ""


toString : Dice -> String
toString dice =
    internalAsString
        |> StaticArray.get dice
