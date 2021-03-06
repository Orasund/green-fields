module View.NoDice exposing (..)

import Data.Route as Route
import Gen.Route as Route
import Html.Styled as Html exposing (Html)
import View.Style as Style


view : Html msg
view =
    Style.section "No Dice"
        [ [ "You don't have any dice. " |> Html.text
          , "Go back into the " |> Html.text
          , Style.link Route.Kitchen
          , " and get some new dice." |> Html.text
          ]
            |> Style.row
        ]
