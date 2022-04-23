module Pages.Home_ exposing (view)

import Config
import Gen.Route as Route exposing (Route(..))
import Html
import Html.Attributes as Attr
import Shared exposing (Msg)
import View exposing (View)
import View.Style as Style


view : View msg
view =
    { title = Config.title
    , body =
        [ Style.section ("Welcome to " ++ Config.title)
            [ Style.paragraph "Hello there, stranger :)"
            , "This is a digital Roll & Write game. "
                ++ "This means you throw dice and then use them for various actions. "
                ++ "The theme of this game is farming, mining and crafting. "
                ++ "If this is up your allay, "
                ++ "click on the link below to head over to the kitchen and start throwing dice."
                |> Style.paragraph
            , Style.link "To the Kitchen" Route.Kitchen
            ]
        ]
    }
