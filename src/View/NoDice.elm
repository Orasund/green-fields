module View.NoDice exposing (..)

import Gen.Route as Route
import Html.Styled as Html exposing (Html)
import View.Style as Style


view : Html msg
view =
    Style.section "No Dice"
        [ "You don't have any dice. "
            ++ "Go back into the kitchen and get some new dice."
            |> Style.paragraph
        , Style.link "To the Kitchen" Route.Kitchen
        ]
