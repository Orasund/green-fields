module Data.Food.Fish exposing (..)


type Fish
    = Bass
    | Trout
    | Salmon


asList : List Fish
asList =
    [ Bass, Trout, Salmon ]


toString : Fish -> String
toString fish =
    case fish of
        Bass ->
            "Bass"

        Trout ->
            "Trout"

        Salmon ->
            "Salmon"


price : Fish -> Int
price fish =
    case fish of
        Bass ->
            10

        Trout ->
            20

        Salmon ->
            40


fromStreet : Int -> Maybe Fish
fromStreet streetLength =
    if streetLength == 3 then
        Just Bass

    else if streetLength == 4 then
        Just Trout

    else if streetLength == 5 then
        Just Salmon

    else
        Nothing


toAmount : Fish -> Int
toAmount fish =
    case fish of
        Bass ->
            1

        Trout ->
            2

        Salmon ->
            3
