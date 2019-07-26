module Bot exposing (Bot, BotControl(..), BotDist, BotDistDict, Color, Vec, aBotDist, botDist, distDict, getBddDist, vecPlus)

import Array as A exposing (Array)
import Dict exposing (Dict)
import EvalStep exposing (EvalBodyStep(..), Term(..))


type alias Color =
    ( Float, Float, Float )


type alias Vec =
    ( Float, Float )


vecPlus : Vec -> Vec -> Vec
vecPlus ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


type alias Bot =
    { programText : String
    , name : String
    , program : Result String (List (Term BotControl))
    , step : EvalBodyStep BotControl
    , position : Vec
    , velocity : Vec
    , accel : Vec
    , dead : Bool
    }


type alias BotDist =
    { d2 : Float
    , d : Float
    }


type alias BotDistDict =
    Dict ( Int, Int ) BotDist


type BotControl
    = BotControl
        { botidx : Int
        , bots : Array Bot
        , bdd : BotDistDict
        , prints : Dict Int (List String)
        }


aBotDist : Int -> Int -> Array Bot -> Maybe BotDist
aBotDist b1 b2 bots =
    case ( A.get b1 bots, A.get b2 bots ) of
        ( Just bot1, Just bot2 ) ->
            botDist bot1 bot2

        _ ->
            Nothing


botDist : Bot -> Bot -> Maybe BotDist
botDist b1 b2 =
    if b1.dead || b2.dead then
        Nothing

    else
        let
            ( x1, y1 ) =
                b1.position

            ( x2, y2 ) =
                b2.position

            dx =
                x2 - x1

            dy =
                y2 - y1

            d2 =
                dx * dx + dy * dy
        in
        Just
            { d2 = d2
            , d = sqrt d2
            }


{-| an dict of (idx1, idx2) -> botdist, where (idx1 < idx2).
if a distance isn't there then either its an invalid bot index, or one or
both of the bots are dead.
-}
distDict : Array Bot -> BotDistDict
distDict bots =
    let
        cm1 =
            A.length bots - 1
    in
    List.foldr
        (\i1 bd1 ->
            List.foldr
                (\i2 bots2 ->
                    case ( A.get i1 bots, A.get i2 bots ) of
                        ( Just b1, Just b2 ) ->
                            case botDist b1 b2 of
                                Just bd ->
                                    Dict.insert ( i1, i2 ) bd bots2

                                Nothing ->
                                    bots2

                        _ ->
                            bots2
                )
                bd1
                (List.range (i1 + 1) cm1)
        )
        Dict.empty
        (List.range 0 (cm1 - 1))


{-| 'Nothing' indicates either invalid bot range, or one or both bots are dead
-}
getBddDist : BotDistDict -> Int -> Int -> Maybe BotDist
getBddDist bdd bid1 bid2 =
    let
        bp =
            if bid1 <= bid2 then
                ( bid1, bid2 )

            else
                ( bid2, bid1 )
    in
    Dict.get bp bdd
