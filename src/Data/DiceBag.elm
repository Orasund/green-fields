module Data.DiceBag exposing (..)

import Data.Dice as Dice exposing (Dice)
import Random exposing (Generator)
import StaticArray exposing (StaticArray)
import StaticArray.Index exposing (Index, Six)
import StaticArray.Length as Length


type alias DiceBag =
    StaticArray Six Int


empty : DiceBag
empty =
    StaticArray.fromList (Length.plus1 Length.five) 0 []


isEmpty : DiceBag -> Bool
isEmpty diceBag =
    diceBag
        |> toList
        |> List.isEmpty


size : DiceBag -> Int
size diceBag =
    diceBag
        |> toList
        |> List.map Tuple.second
        |> List.sum


count : Dice -> DiceBag -> Int
count dice =
    StaticArray.get dice


addAll : List Dice -> DiceBag -> DiceBag
addAll list diceBag =
    list
        |> List.foldl add diceBag


removeAll : List Dice -> DiceBag -> DiceBag
removeAll list diceBag =
    list
        |> List.foldl remove diceBag


add : Dice -> DiceBag -> DiceBag
add =
    addN 1


addN : Int -> Dice -> DiceBag -> DiceBag
addN n i bag =
    bag |> StaticArray.set i (bag |> StaticArray.get i |> (+) n)


remove : Dice -> DiceBag -> DiceBag
remove i bag =
    bag |> StaticArray.set i (bag |> StaticArray.get i |> (+) -1 |> max 0)


toList : DiceBag -> List ( Dice, Int )
toList diceBag =
    diceBag
        |> StaticArray.indexedMap (\i n -> ( i, n ))
        |> StaticArray.toList
        |> Debug.log "after indexedMap"
        |> List.filter (\( _, n ) -> n > 0)


toString : DiceBag -> String
toString diceBag =
    diceBag
        |> toList
        |> List.map
            (\( i, n ) ->
                List.repeat n i
                    |> List.map Dice.toString
                    |> String.concat
            )
        |> String.join " "
