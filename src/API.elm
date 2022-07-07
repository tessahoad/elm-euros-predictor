module API exposing (..)

import Json.Decode as Decode exposing (Decoder, field, map, map2, map3)
import Json.Decode.Pipeline exposing (optional, required)


serverEndpoint : String -> String
serverEndpoint resource =
    "http://localhost:8080/" ++ resource


type alias Team =
    { name : String
    , rating : Int
    }


type alias TeamStats =
    { name : String
    , wins : Int
    , losses : Int
    , draws : Int
    , points : Int
    }


type alias Group =
    { name : String
    , teams : List Team
    , fixtures : List Fixture
    }


type alias Fixture =
    { homeTeam : String
    , awayTeam : String
    , result : Maybe GameResult
    , round : Round
    }


type alias FixtureResult =
    { result : GameResult
    }


type GameResult
    = HomeWin
    | Draw
    | HomeLoss


type Round
    = Round1
    | Round2
    | Round3
    | QuarterFinal
    | SemiFinal
    | Final


decodeTeam : Decoder Team
decodeTeam =
    map2 Team
        (field "name" Decode.string)
        (field "rating" Decode.int)


decodeTeams : Decoder (List Team)
decodeTeams =
    Decode.list decodeTeam


decodeGameResult : Decoder (Maybe GameResult)
decodeGameResult =
    Decode.string
        |> Decode.andThen
            (\resultString ->
                case resultString of
                    "HomeWin" ->
                        Decode.succeed (Just HomeWin)

                    "Draw" ->
                        Decode.succeed (Just Draw)

                    "HomeLoss" ->
                        Decode.succeed (Just HomeLoss)

                    _ ->
                        Decode.succeed Nothing
            )


decodeRound : Decoder Round
decodeRound =
    Decode.string
        |> Decode.andThen
            (\roundString ->
                case roundString of
                    "Round1" ->
                        Decode.succeed Round1

                    "Round2" ->
                        Decode.succeed Round2

                    "Round3" ->
                        Decode.succeed Round3

                    _ ->
                        Decode.fail roundString
            )


decodeFixture : Decoder Fixture
decodeFixture =
    Decode.succeed Fixture
        |> required "homeTeam" Decode.string
        |> required "awayTeam" Decode.string
        |> optional "result" decodeGameResult Nothing
        |> required "round" decodeRound


decodeFixtures : Decoder (List Fixture)
decodeFixtures =
    Decode.list decodeFixture


decodeGroup : Decoder Group
decodeGroup =
    Decode.succeed Group
        |> required "name" Decode.string
        |> required "teams" decodeTeams
        |> required "fixtures" decodeFixtures


decodeGroups : Decoder (List Group)
decodeGroups =
    Decode.list decodeGroup
