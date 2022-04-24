module Data.DiceBag exposing (..)

import Data.Die as Dice exposing (Die)
import Random exposing (Generator)
import StaticArray exposing (StaticArray)
import StaticArray.Index as Index exposing (Index, Six)
import StaticArray.Length as Length


type alias DiceBag =
    StaticArray Six Int


empty : DiceBag
empty =
    StaticArray.fromList (Length.plus1 Length.five) ( 0, [] )


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


count : Die -> DiceBag -> Int
count dice =
    StaticArray.get dice


member : Die -> DiceBag -> Bool
member die diceBag =
    count die diceBag > 0


addAll : List Die -> DiceBag -> DiceBag
addAll list diceBag =
    diceBag
        |> addAllN
            (list
                |> List.map (\dice -> ( dice, 1 ))
            )


addAllN : List ( Die, Int ) -> DiceBag -> DiceBag
addAllN list diceBag =
    list
        |> List.foldl (\( i, n ) -> addN n i)
            diceBag


removeAll : List Die -> DiceBag -> DiceBag
removeAll list diceBag =
    list
        |> List.foldl remove diceBag


addN : Int -> Die -> DiceBag -> DiceBag
addN n i bag =
    bag |> StaticArray.set i (bag |> StaticArray.get i |> (+) n)


remove : Die -> DiceBag -> DiceBag
remove i bag =
    bag |> StaticArray.set i (bag |> StaticArray.get i |> (+) -1 |> max 0)


fromList : List ( Die, Int ) -> DiceBag
fromList list =
    empty
        |> addAllN list


toList : DiceBag -> List ( Die, Int )
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


streets : DiceBag -> List (List Die)
streets bag =
    case toList bag of
        [] ->
            []

        ( head, _ ) :: tail ->
            tail
                |> List.foldl
                    (\( i, _ ) ( ( current, street ), list ) ->
                        if Index.increase Dice.length current == Just i then
                            ( ( i, current :: street ), list )

                        else
                            ( ( i, [] ), (current :: street) :: list )
                    )
                    ( ( head, [] ), [] )
                |> (\( ( current, street ), list ) -> (current :: street) :: list)
                |> List.filter (\street -> List.length street >= 3)
