module Data.Food exposing (..)


type Fish
    = Bass
    | Trout
    | Salmon


type Food
    = FishFood Fish


toString : Food -> String
toString food =
    case food of
        FishFood fish ->
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


fromStreet : Int -> Maybe Food
fromStreet streetLength =
    Maybe.map FishFood
        (if streetLength == 3 then
            Just Bass

         else if streetLength == 4 then
            Just Trout

         else if streetLength == 5 then
            Just Salmon

         else
            Nothing
        )
