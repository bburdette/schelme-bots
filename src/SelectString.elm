module SelectString exposing (view)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Element.Input as EI



{- dialog/pane displaying a list of strings and allowing the user to select one and
   click ok, or dismiss by canceling or clicking outside the pane.
-}


type alias Model =
    { title : String
    , choices : List String
    , selected : Maybe Int
    }


type Command
    = Noop
    | Canceled
    | Selected String


type Msg
    = Select Int
    | Pick String
    | Cancel


update : Model -> Msg -> ( Model, Command )
update model msg =
    case msg of
        Cancel ->
            ( { model | selected = Nothing }, Canceled )

        Select idx ->
            ( { model | selected = Just idx }, Noop )

        Pick s ->
            ( { model | selected = Nothing }, Selected s )


view : Model -> Element ( Model, Command )
view model =
    column [ BG.color <| rgb 1 1 1 ] <|
        (el [ Font.bold ] <| text model.title)
            :: List.indexedMap
                (\i s ->
                    row
                        [ EE.onMouseDown (update model (Select i))
                        , EE.onMouseUp (update model (Pick s))
                        ]
                        [ text s ]
                )
                model.choices



{- view : String -> List String -> (String -> m) -> m -> Element m
   view title strings selectmsg cancelmsg =
       column [ BG.color <| rgb 1 1 1 ] <|
           (el [ Font.bold ] <| text title)
               :: List.indexedMap (\i s -> row [ EE.onClick (selectmsg s) ] [ text s ]) strings
-}
