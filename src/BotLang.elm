module BotLang exposing (Bot, BotControl(..), BotDist, BotDistDict, Color, Vec, aBotDist, allreference, botDist, botftns, botlang, botreference, distDict, fromPolar, getBddDist, getOpIdx, getPosition, getVelocity, myPosition, myVelocity, opponentCount, print, setThrust, toPolar, vecPlus)

import Array as A exposing (Array)
import Dict exposing (Dict)
import EvalStep exposing (EvalBodyStep(..), GlossaryEntry, NameSpace, Term(..), TermGlossary)
import Prelude as Prelude exposing (BuiltInFn, evalArgsBuiltIn, evalArgsSideEffector)
import Show exposing (showTerm, showTerms)


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
getBddDist : Int -> Int -> BotDistDict -> Maybe BotDist
getBddDist bid1 bid2 bdd =
    let
        bp =
            if bid1 <= bid2 then
                ( bid1, bid2 )

            else
                ( bid2, bid1 )
    in
    Dict.get bp bdd


botftns =
    Dict.empty
        |> Dict.insert "print" (TSideEffector (evalArgsSideEffector print))
        |> Dict.insert "setThrust" (TSideEffector (evalArgsSideEffector setThrust))
        |> Dict.insert "opponentCount" (TBuiltIn (evalArgsBuiltIn opponentCount))
        |> Dict.insert "getPosition" (TBuiltIn (evalArgsBuiltIn getPosition))
        |> Dict.insert "myPosition" (TBuiltIn (evalArgsBuiltIn myPosition))
        |> Dict.insert "getVelocity" (TBuiltIn (evalArgsBuiltIn getVelocity))
        |> Dict.insert "myVelocity" (TBuiltIn (evalArgsBuiltIn myVelocity))
        |> Dict.insert "toPolar" (TBuiltIn (evalArgsBuiltIn toPolar))
        |> Dict.insert "fromPolar" (TBuiltIn (evalArgsBuiltIn fromPolar))


botreference : TermGlossary
botreference =
    Dict.empty
        |> Dict.insert "print"
            (GlossaryEntry
                "(print <expression>) -> ()"
                "prints a debug message"
            )
        |> Dict.insert "setThrust"
            (GlossaryEntry
                "(setThrust <radians> <acceleration>"
                "set direction and amount of acceleration"
            )
        |> Dict.insert "opponentCount"
            (GlossaryEntry
                "(opponentCount) -> <number>"
                "returns the number of live opponents"
            )
        |> Dict.insert "getPosition"
            (GlossaryEntry
                "(getPosition <num index>) -> (<num x>, <num y>)"
                "returns the XY position of an opponent"
            )
        |> Dict.insert "myPosition"
            (GlossaryEntry
                "(myPosition) -> (<num x>, <num y>)"
                "returns the XY position of the 'self' bot"
            )
        |> Dict.insert "getVelocity"
            (GlossaryEntry
                "(getVelocity <num index>) -> (<num x>, <num y>)"
                "given an index, returns the XY vector of the opponent's velocity."
            )
        |> Dict.insert "myVelocity"
            (GlossaryEntry
                "(myVelocity) -> (<num x>, <num y>)"
                "returns the XY velocity vector of 'self'"
            )
        |> Dict.insert "toPolar"
            (GlossaryEntry
                "(toPolar <num x>, <num y>) -> (<radians>, <distance>)"
                "convert XY to Angle,Radius"
            )
        |> Dict.insert "fromPolar"
            (GlossaryEntry
                "(fromPolar <radians>, <distance>) -> (<num x>, <num y>)"
                "convert Angle,Radius to XY"
            )


allreference =
    botreference
        |> Dict.union Prelude.preludeGlossary
        |> Dict.union Prelude.mathGlossary


botlang =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.union botftns


fromPolar : BuiltInFn a
fromPolar ns _ terms =
    case terms of
        [ TNumber a, TNumber m ] ->
            Ok ( ns, TList [ TNumber <| cos a * m, TNumber <| sin a * m ] )

        _ ->
            Err ("fromPolar expected two numbers, got: " ++ showTerms terms)



{-
               |
     y / -x    |      y / x
               |
               |
   -----------------------------
               |
               |
    -y / -x    |     -y / x
               |
               |
-}


toPolar : BuiltInFn a
toPolar ns _ terms =
    case terms of
        [ TNumber x, TNumber y ] ->
            let
                a =
                    atan (y / x)
                        + (if x < 0 then
                            pi

                           else
                            0
                          )

                m =
                    sqrt (x * x + y * y)
            in
            Ok ( ns, TList [ TNumber a, TNumber m ] )

        _ ->
            Err ("toPolar expected two numbers, got: " ++ showTerms terms)


{-| includes dead bots!
-}
opponentCount : Prelude.BuiltInFn BotControl
opponentCount ns (BotControl bc) argterms =
    case argterms of
        [] ->
            Ok ( ns, TNumber <| toFloat (A.length bc.bots - 1) )

        _ ->
            Err (String.concat ("opponentCount takes 0 arguments!  " :: List.map showTerm argterms))


getOpIdx : Int -> Int -> Int -> Maybe Int
getOpIdx robot rqidx count =
    let
        i =
            modBy count (1 + rqidx + robot)
    in
    if i == robot then
        Nothing

    else
        Just i


{-| if a bot is dead, returns (list)
-}
getPosition : Prelude.BuiltInFn BotControl
getPosition ns (BotControl bc) argterms =
    case argterms of
        [ TNumber idx ] ->
            let
                opidx =
                    getOpIdx bc.botidx (round idx) (A.length bc.bots)
            in
            case opidx |> Maybe.andThen (\oi -> A.get oi bc.bots) of
                Just bot ->
                    if bot.dead then
                        Ok ( ns, TList [] )

                    else
                        Ok ( ns, TList [ TNumber <| Tuple.first bot.position, TNumber <| Tuple.second bot.position ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("getPosition takes 1 argument!  " :: List.map showTerm argterms))


myPosition : Prelude.BuiltInFn BotControl
myPosition ns (BotControl bc) argterms =
    case argterms of
        [] ->
            case A.get bc.botidx bc.bots of
                Just bot ->
                    Ok ( ns, TList [ TNumber <| Tuple.first bot.position, TNumber <| Tuple.second bot.position ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("myPosition takes 0 arguments!  " :: List.map showTerm argterms))


myVelocity : Prelude.BuiltInFn BotControl
myVelocity ns (BotControl bc) argterms =
    case argterms of
        [] ->
            case A.get bc.botidx bc.bots of
                Just bot ->
                    Ok ( ns, TList [ TNumber <| Tuple.first bot.velocity, TNumber <| Tuple.second bot.velocity ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("myVelocity takes 0 arguments!  Got:" :: List.map showTerm argterms))


getVelocity : Prelude.BuiltInFn BotControl
getVelocity ns (BotControl bc) argterms =
    case argterms of
        [ TNumber idx ] ->
            let
                opidx =
                    getOpIdx bc.botidx (round idx) (A.length bc.bots)
            in
            case opidx |> Maybe.andThen (\oi -> A.get oi bc.bots) of
                Just bot ->
                    if bot.dead then
                        Ok ( ns, TList [] )

                    else
                        Ok ( ns, TList [ TNumber <| Tuple.first bot.velocity, TNumber <| Tuple.second bot.velocity ] )

                Nothing ->
                    Ok ( ns, TList [] )

        _ ->
            Err (String.concat ("getVelocity takes 1 argument!  Got:" :: List.map showTerm argterms))


setThrust : Prelude.SideEffectorFn BotControl
setThrust ns (BotControl bc) argterms =
    case argterms of
        [ TNumber angle, TNumber power ] ->
            let
                p =
                    max 0.0 (min 1.0 power)
            in
            case A.get bc.botidx bc.bots of
                Just bot ->
                    Ok ( ns, BotControl { bc | bots = A.set bc.botidx { bot | accel = ( cos angle * p, sin angle * p ) } bc.bots }, TList [] )

                Nothing ->
                    Err ("bot not found at index: " ++ String.fromInt bc.botidx)

        _ ->
            Err (String.concat ("thrust takes 2 arguments!  " :: List.map showTerm argterms))


print : Prelude.SideEffectorFn BotControl
print ns (BotControl bc) argterms =
    {- let
       _ = Debug.log ("bot " ++ String.fromInt bc.botidx ++ " printed: ") <| showTerms argterms
           in
    -}
    Ok
        ( ns
        , BotControl
            { bc
                | prints =
                    Dict.get bc.botidx bc.prints
                        |> Maybe.withDefault []
                        |> (::) (showTerms argterms)
                        |> List.take 20
                        |> (\strs -> Dict.insert bc.botidx strs bc.prints)
            }
        , TList []
        )
