module Data.Stone exposing (..)

import Config
import Gen.Enum.Stone as Stone


type alias Stone =
    Stone.Stone


default : Stone
default =
    Stone.first


last : Stone
last =
    Stone.last


fromInt : Int -> Maybe Stone
fromInt =
    Stone.fromInt


toInt : Stone -> Int
toInt =
    Stone.toInt


toEmoji : Stone -> String
toEmoji stone =
    case stone of
        Stone.Limestone ->
            "\u{1FAA8}"


toString : Stone -> String
toString stone =
    toEmoji stone ++ Stone.toString stone


price : Stone -> Int
price stone =
    case stone of
        Stone.Limestone ->
            Config.priceOfStone


description : Stone -> String
description stone =
    case stone of
        Stone.Limestone ->
            "Add a common item to a pool"
