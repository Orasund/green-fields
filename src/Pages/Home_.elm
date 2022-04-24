module Pages.Home_ exposing (view)

import Config
import Css
import Gen.Route as Route exposing (Route(..))
import Html.Attributes as Attr
import Html.Styled as Html
import Shared exposing (Msg)
import View exposing (View)
import View.Style as Style


view : View msg
view =
    { title = "Welcome to " ++ Config.title
    , body =
        [ Style.paragraph "Hello there, stranger :)"
        , [ "This is a digital Roll & Write game. "
                ++ "This means you throw dice and then use them for various actions. "
                ++ "The theme of this game is farming, mining and crafting. "
                ++ "If this is up your allay, head over to the "
                |> Html.text
          , Style.link Route.Kitchen
          , " and start rolling some dice." |> Html.text
          ]
            |> Style.row
        ]
    }
