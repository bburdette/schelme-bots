module OkayDialog exposing (Command(..), Model, Msg(..), okayDialog)

import Dialog exposing (DMsg(..), Dialog(..))
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as EE
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

                        Cancel ->
                            Canceled

                        Noop ->
                            None
            in
            ( Dialog (okayDialog model), cmd )



{- dialog/pane displaying a list of strings and allowing the user to select one and
   click ok, or dismiss by canceling or clicking outside the pane.
-}


type alias Model =
    { title : String
    , message : String
    , buttonStyle : List (Element.Attribute ())
    }


type Command
    = Canceled
    | Okayed
    | None


type Msg
    = Okay
    | Cancel
    | Noop


view : Model -> Element Msg
view model =
    let
        bs =
            List.map (Element.mapAttribute (always Noop))
                model.buttonStyle

        -- Element.map (always Noop) model.buttonStyle
    in
    column [ BG.color <| rgb 1 1 1 ]
        [ el [ Font.bold ] <| text model.title
        , text model.message
        , EI.button (alignLeft :: bs)
            { onPress = Just <| Okay
            , label = text "Ok"
            }
        , EI.button (alignRight :: bs)
            { onPress = Just <| Cancel
            , label = text "Cancel"
            }
        ]
