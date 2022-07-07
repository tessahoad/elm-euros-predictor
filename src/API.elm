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
    , position : Int
    }


type alias Group =
    { name : String
    , teams : List Team
    , fixtures : List Fixture
    }


type alias Fixture =
    { homeTeam : String
    , awayTeam : String
    , result : String
    , round : String
    }


type alias FixtureResult =
    { result : Result
    }


type Result
    = HomeWin
    | Draw
    | HomeLoss
    | Unplayed


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


decodeFixture : Decoder Fixture
decodeFixture =
    Decode.succeed Fixture
        |> required "homeTeam" Decode.string
        |> required "awayTeam" Decode.string
        |> optional "result" Decode.string "Unplayed"
        |> required "round" Decode.string


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
