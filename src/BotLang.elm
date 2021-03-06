module BotLang exposing (allreference, botftns, botlang, botreference, fromOpponentIndex, fromPolar, getBotDistances, getPosition, getVelocity, myPosition, myVelocity, opponentCount, print, setThrust, toOpponentIdx, toPolar)

import Array as A
import Bot exposing (BotControl(..))
import Dict
import EvalStep exposing (EvalBodyStep(..), GlossaryEntry, NameSpace, Term(..), TermGlossary)
import Prelude as Prelude exposing (BuiltInFn, evalArgsBuiltIn, evalArgsSideEffector)
import Show exposing (showTerm, showTerms)


botftns : NameSpace BotControl
botftns =
    Dict.empty
        |> Dict.insert "print" (TSideEffector (evalArgsSideEffector print))
        |> Dict.insert "setThrust" (TSideEffector (evalArgsSideEffector setThrust))
        |> Dict.insert "opponentCount" (TBuiltIn (evalArgsBuiltIn opponentCount))
        |> Dict.insert "getBotDistances" (TBuiltIn (evalArgsBuiltIn getBotDistances))
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
        |> Dict.insert "getBotDistances"
            (GlossaryEntry
                "(getBotDistances) -> list (<num index>, <num distance>)"
                "return a list of bot indexes and distances, with closest bots first"
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


allreference : TermGlossary
allreference =
    botreference
        |> Dict.union Prelude.preludeGlossary
        |> Dict.union Prelude.mathGlossary


botlang : NameSpace BotControl
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
   ---------------------------
               |
               |
    -y / -x    |     -y / x
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


{-| in robot language, opponent index 0 is the robot's index + 1, and goes up to
count - 1. This is so all scripts don't target the same bot if they specify bot 0.

0 1 2 3 4 5 'actual' index in Array Bot
....r
3 4 x 0 1 2 indexes used by bot script.

0 1 2 3 4 5 6 'actual' index in Array Bot
....r
4 5 x 0 1 2 3 indexes used by bot script.

-}
toOpponentIdx : Int -> Int -> Int -> Maybe Int
toOpponentIdx robotidx actualidx count =
    case compare robotidx actualidx of
        LT ->
            Just <| (actualidx - robotidx - 1)

        EQ ->
            Nothing

        GT ->
            Just <| actualidx + (count - robotidx - 1)


{-| convert from opponent index to elm array index
-}
fromOpponentIndex : Int -> Int -> Int -> Maybe Int
fromOpponentIndex robot rqidx count =
    let
        i =
            modBy count (1 + rqidx + robot)
    in
    if i == robot then
        Nothing

    else
        Just i


getBotDistances : Prelude.BuiltInFn BotControl
getBotDistances ns (BotControl bc) argterms =
    case argterms of
        [] ->
            let
                bdists =
                    List.filterMap
                        (\( idx, dist ) ->
                            toOpponentIdx bc.botidx idx (A.length bc.bots)
                                |> Maybe.map (\i -> ( i, dist ))
                        )
                    <|
                        Bot.closestBots bc.bdd bc.botidx (A.length bc.bots)
            in
            Ok ( ns, TList (List.map (\( a, b ) -> TList [ TNumber (toFloat a), TNumber b ]) bdists) )

        _ ->
            Err (String.concat ("getBotDistances takes 0 arguments!  " :: List.map showTerm argterms))


{-| returns a list of bot indexes in order of proximity.
-}
getPosition : Prelude.BuiltInFn BotControl
getPosition ns (BotControl bc) argterms =
    case argterms of
        [ TNumber idx ] ->
            let
                opidx =
                    fromOpponentIndex bc.botidx (round idx) (A.length bc.bots)
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
                    fromOpponentIndex bc.botidx (round idx) (A.length bc.bots)
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
