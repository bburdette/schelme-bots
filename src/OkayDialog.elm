module OkayDialog exposing (Command(..), Model, Msg(..), okayDialog)

import Dialog exposing (DMsg(..), Dialog(..))
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Element.Input as EI


okayDialog : Model -> DMsg Msg -> ( Dialog Msg Command, Command )
okayDialog model dmsg =
    case dmsg of
        Render ->
            ( Rendering (view model), None )

        Dialog.Msg msg ->
            let
                cmd =
                    case msg of
                        Okay ->
                            Okayed

                        Noop ->
                            None
            in
            ( Dialog (okayDialog model), cmd )


type alias Model =
    { title : String
    , message : String
    , buttonStyle : List (Element.Attribute ())
    }


type Command
    = Okayed
    | None


type Msg
    = Okay
    | Noop


view : Model -> Element Msg
view model =
    let
        bs =
            List.map (Element.mapAttribute (always Noop))
                model.buttonStyle
    in
    column [ BG.color <| rgb 1 1 1, spacing 10 ]
        [ el [ Font.bold, centerX ] <| text model.title
        , el [ centerX ] <| text <| "\"" ++ model.message ++ "\""
        , EI.button (centerX :: bs)
            { onPress = Just <| Okay
            , label = text "Ok"
            }
        ]
