module Data.AnyBag exposing (..)

import Dict.Any as AnyDict exposing (AnyDict)


type alias AnyBag comparable k =
    AnyDict comparable k Int


empty : (k -> comparable) -> AnyBag comparable k
empty =
    AnyDict.empty


insert : Int -> k -> AnyBag comparable k -> AnyBag comparable k
insert n k bag =
    bag
        |> AnyDict.update k
            (\maybe ->
                if n > 0 then
                    case maybe of
                        Nothing ->
                            Just n

                        Just m ->
                            Just (n + m)

                else
                    case maybe of
                        Nothing ->
                            Nothing

                        Just m ->
                            m - n |> max 0 |> Just
            )


remove : Int -> k -> AnyBag comparable k -> AnyBag comparable k
remove n =
    insert -n


toList : AnyBag comparable k -> List ( k, Int )
toList =
    AnyDict.toList
