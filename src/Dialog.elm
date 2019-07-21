module Dialog exposing (DMsg(..), Dialog(..), dmMap, duMap, render)

import Element exposing (Element)


type DMsg msg
    = Msg msg
    | Render


type Dialog msg cmd
    = Dialog (DMsg msg -> ( Dialog msg cmd, cmd ))
    | Rendering (Element msg)


render : Dialog msg cmd -> Maybe (Element msg)
render dlg =
    case dlg of
        Rendering elt ->
            Just elt

        Dialog dlg2 ->
            case dlg2 Render of
                ( Rendering elt, _ ) ->
                    Just elt

                _ ->
                    Nothing


dmMap : DMsg a -> (a -> b) -> DMsg b
dmMap da fab =
    case da of
        Msg ma ->
            Msg (fab ma)

        Render ->
            Render


duMap : Dialog am ac -> (bm -> am) -> (am -> bm) -> (ac -> bc) -> Dialog bm bc
duMap dlg inmap outmap resmap =
    case dlg of
        Dialog dupA ->
            Dialog
                (\dmsgB ->
                    let
                        ( newDupA, rmsg ) =
                            dupA (dmMap dmsgB inmap)
                    in
                    ( duMap newDupA inmap outmap resmap, resmap rmsg )
                )

        Rendering elt ->
            Rendering (Element.map outmap elt)
