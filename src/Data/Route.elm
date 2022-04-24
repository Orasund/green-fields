module Data.Route exposing (..)

import Gen.Route as Route exposing (Route(..))


toString : Route -> String
toString route =
    case route of
        Fields ->
            "🌱Fields"

        Home_ ->
            "🏡Home"

        Kitchen ->
            "🏡Kitchen"

        Lake ->
            "🎣Lake"

        Mine ->
            "⛏️Mine"

        Woods ->
            "🪓Woods"

        NotFound ->
            "⛔Not Found"
