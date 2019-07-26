module SelectStringDialog exposing (Command(..), Model, Msg(..), ssDialog, update, view)

import Dialog exposing (DMsg(..), Dialog(..))
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
    = None
    | Canceled
    | Selected String


type Msg
    = Select Int
    | Pick String
    | Cancel
    | Noop


update : Model -> Msg -> ( Model, Command )
update model msg =
    case msg of
        Cancel ->
            ( { model | selected = Nothing }, Canceled )

        Select idx ->
            ( { model | selected = Just idx }, None )

        Pick s ->
            ( { model | selected = Nothing }, Selected s )

        Noop ->
            ( model, None )


view : Model -> Element Msg
view model =
    column [ BG.color <| rgb 1 1 1 ] <|
        (el [ Font.bold ] <| text model.title)
            :: List.indexedMap
                (\i s ->
                    row
                        [ EE.onMouseDown (Select i)
                        , EE.onMouseUp (Pick s)
                        , BG.color
                            (if model.selected == Just i then
                                rgb 0.5 0.5 0.5

                             else
                                rgb 1 1 1
                            )
                        ]
                        [ text s ]
                )
                model.choices


ssDialog : Model -> DMsg Msg -> ( Dialog Msg Command, Command )
ssDialog model dmsg =
    case dmsg of
        Render ->
            ( Rendering (view model), None )

        Dialog.Msg msg ->
            let
                ( nm, cmd ) =
                    update model msg
            in
            ( Dialog (ssDialog nm), cmd )
