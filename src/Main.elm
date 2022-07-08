module Main exposing (..)

import API exposing (Fixture, GameResult(..), Group, Round(..), Team, TeamStats, decodeGroups, decodeTeams, serverEndpoint)
import Browser
import Bulma.CDN exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Debug exposing (..)
import Html exposing (Html, main_, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Random


type Msg
    = FetchGroups
    | FetchGroupsResult (Result Http.Error (List Group))
    | GroupTabClick String
    | SimulateRound
    | NewRandomNumber Float


type alias Model =
    { ratings : List Team
    , groups : List Group
    , activeGroupTab : String
    , randomFloat : Float
    , lastPlayedRound : Maybe Round
    , quarterFinalFixtures: Maybe (List Fixture)
    , semiFinalFixtures: Maybe (List Fixture)
    , finalFixture: Maybe Fixture
    }


{-| This is the initial state of the system
-}
init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { ratings = []
            , groups = []
            , activeGroupTab = "A"
            , randomFloat = 0.5
            , lastPlayedRound = Nothing
            , quarterFinalFixtures = Nothing
            , semiFinalFixtures = Nothing
            , finalFixture = Nothing
            }
    in
    ( model, Cmd.batch [ getGroups, Random.generate NewRandomNumber (Random.float 0 1) ] )


{-| This is the update function that mutates the state of the system
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGroups ->
            ( model, getGroups )

        FetchGroupsResult (Ok groups) ->
            ( { model | groups = groups }, Cmd.none )

        GroupTabClick groupName ->
            ( { model | activeGroupTab = groupName }, Cmd.none )

        SimulateRound ->
            ( playRound model, Random.generate NewRandomNumber (Random.float 0 1) )

        NewRandomNumber number ->
            ( { model | randomFloat = number }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- View


{-| This is view function that renders the model as HTML
-}
view : Model -> Html Msg
view model =
    main_ []
        [ stylesheet
        , navbar myNavbarModifiers [] []
        , section NotSpaced
            []
            [ tileAncestor Auto
                []
                [ tileParent Width8
                    [ class "notification is-warning" ]
                    [ verticalTile Auto
                        []
                        [ p [ class "title" ] [ text "Quarter Finals" ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "Group A"
                                , levelItemText [ class "has-text-centered" ] [ text "v" ]
                                , easyLevelItemWithHeading [ class "has-text-centered" ] "Runner Up" "Group B"
                                ]
                            ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "Group B"
                                , levelItemText [ class "has-text-centered" ] [ text "v" ]
                                , easyLevelItemWithHeading [ class "has-text-centered" ] "Runner Up" "Group A"
                                ]
                            ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "Group C"
                                , levelItemText [ class "has-text-centered" ] [ text "v" ]
                                , easyLevelItemWithHeading [ class "has-text-centered" ] "Runner Up" "Group D"
                                ]
                            ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "Group D"
                                , levelItemText [ class "has-text-centered" ] [ text "v" ]
                                , easyLevelItemWithHeading [ class "has-text-centered" ] "Runner Up" "Group C"
                                ]
                            ]
                        ]
                    , verticalTile Auto
                        []
                        [ p [ class "title" ] [ text "Semi Finals" ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "QF 1"
                                , levelItemText [ class "has-text-centered" ] [ text "v" ]
                                , easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "QF 2"
                                ]
                            ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "QF 3"
                                , levelItemText [ class "has-text-centered" ] [ text "v" ]
                                , easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "QF 4"
                                ]
                            ]
                        ]
                    , verticalTile Auto
                        []
                        [ p [ class "title" ] [ text "Final" ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "SF 1"
                                , levelItemText [ class "has-text-centered" ] [ text "v" ]
                                , easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "SF 2"
                                ]
                            ]
                        , tileChild Auto
                            []
                            [ level []
                                [ easyLevelItemWithHeading [ class "has-text-centered" ] "Winner" "TBD"
                                ]
                            ]
                        ]
                    ]
                , verticalTileParent Auto
                    []
                    [ tileChild Auto
                        [ class "notification is-info" ]
                        [ groupsTabs model
                        , groupsContent model
                        ]
                    , tileChild Auto
                        []
                        [ tileParent Auto
                            []
                            [ tileChild Auto
                                []
                                [ button myButtonModifiers [ onClick SimulateRound ] [ text "Simulate Round" ]
                                ]
                            , tileChild Auto
                                []
                                [ button myButtonModifiers [] [ text "Simulate Tournament" ]
                                ]
                            ]
                        , tileParent Auto
                            []
                            [ tileChild Auto
                                []
                                [--[ button myButtonModifiers [] [ text "It's Coming Home" ]
                                ]
                            , tileChild Auto
                                []
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]


groupsTabs : Model -> Tabs Msg
groupsTabs model =
    tabs myTabsModifiers [ class "is-fullwidth" ] [] (List.map (groupTab model.activeGroupTab) model.groups)


groupTab : String -> Group -> Tab Msg
groupTab activeTabName group =
    let
        isActive =
            group.name == activeTabName
    in
    tab isActive [ onClick (GroupTabClick group.name) ] [] [ text group.name ]


groupsContent : Model -> Html Msg
groupsContent model =
    table myTableModifiers
        []
        [ tableHead [] [ groupsContentTableHead ]
        , groupsContentTableBody model
        , tableFoot [] []
        ]


groupsContentTableHead : Html Msg
groupsContentTableHead =
    tableRow False
        []
        [ tableCellHead [] [ text "Pos." ]
        , tableCellHead [] [ text "Name" ]
        , tableCellHead [] [ text "Pld" ]
        , tableCellHead [] [ text "W" ]
        , tableCellHead [] [ text "D" ]
        , tableCellHead [] [ text "L" ]
        , tableCellHead [] [ text "Pts" ]
        ]


groupsContentTableBody : Model -> Html Msg
groupsContentTableBody model =
    let
        activeGroup =
            List.head (List.filter (groupNameMatch model.activeGroupTab) model.groups)
    in
    case activeGroup of
        Just group ->
            let
                groupTeamsByPoints = teamsOrderedByPoints group
                teamsWithPositions = List.indexedMap Tuple.pair groupTeamsByPoints
            in
            tableBody [] (List.map teamRow teamsWithPositions)

        Nothing ->
            tableRow False [] []


teamsOrderedByPoints : Group -> List TeamStats
teamsOrderedByPoints group =
    List.reverse (List.sortBy .points (listTeamStats group))


listTeamStats : Group -> List TeamStats
listTeamStats group =
    List.map (getTeamStats group.fixtures) group.teams


getTeamStats : List Fixture -> Team -> TeamStats
getTeamStats fixtures team =
    let
        playedFixtures =
            List.filter isPlayedFixture fixtures

        playedHomeFixtures =
            List.filter (isHomeFixture team.name) playedFixtures

        playedAwayFixtures =
            List.filter (isAwayFixture team.name) playedFixtures

        wins =
            getHomeWins playedHomeFixtures + getAwayWins playedAwayFixtures

        draws =
            getDraws playedHomeFixtures + getDraws playedAwayFixtures

        losses =
            (List.length playedHomeFixtures + List.length playedAwayFixtures) - wins - draws
    in
    { name = team.name
    , wins = wins
    , draws = draws
    , losses = losses
    , points = getPoints wins draws
    }


isPlayedFixture : Fixture -> Bool
isPlayedFixture fixture =
    fixture.result /= Nothing


isUnplayedFixture : Fixture -> Bool
isUnplayedFixture fixture =
    fixture.result == Nothing


isHomeFixture : String -> Fixture -> Bool
isHomeFixture teamName fixture =
    fixture.homeTeam == teamName


isAwayFixture : String -> Fixture -> Bool
isAwayFixture teamName fixture =
    fixture.awayTeam == teamName


getHomeWins : List Fixture -> Int
getHomeWins fixtures =
    List.length (List.filter isHomeWin fixtures)


getAwayWins : List Fixture -> Int
getAwayWins fixtures =
    List.length (List.filter isAwayWin fixtures)


getDraws : List Fixture -> Int
getDraws fixtures =
    List.length (List.filter isDraw fixtures)


isHomeWin : Fixture -> Bool
isHomeWin fixture =
    fixture.result == Just HomeWin


isAwayWin : Fixture -> Bool
isAwayWin fixture =
    fixture.result == Just HomeLoss


isDraw : Fixture -> Bool
isDraw fixture =
    fixture.result == Just Draw


teamRow : (Int, TeamStats) -> TableRow Msg
teamRow (position, teamStats) =
    tableRow False
        []
        [ tableCell [] [ text (String.fromInt (position + 1)) ]
        , tableCell [] [ text teamStats.name ]
        , tableCell [] [ text (String.fromInt (getPlayed teamStats.wins teamStats.draws teamStats.losses)) ]
        , tableCell [] [ text (String.fromInt teamStats.wins) ]
        , tableCell [] [ text (String.fromInt teamStats.draws) ]
        , tableCell [] [ text (String.fromInt teamStats.losses) ]
        , tableCell [] [ text (String.fromInt teamStats.points) ]
        ]


getPoints : Int -> Int -> Int
getPoints wins draws =
    (wins * 3) + draws


getPlayed : Int -> Int -> Int -> Int
getPlayed wins draws losses =
    wins + draws + losses


groupNameMatch : String -> Group -> Bool
groupNameMatch name group =
    name == group.name



-- Main


{-| This is the entry point which binds the functions to the Elm runtime
-}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


{-| A GET operation to retrieve the groups
-}
getGroups : Cmd Msg
getGroups =
    let
        url =
            serverEndpoint "groups.json"
    in
    Http.get
        { url = url
        , expect = Http.expectJson FetchGroupsResult decodeGroups
        }


getQuarterFinalFixtures : List Group -> List Fixture
getQuarterFinalFixtures groups =
    let
        groupA =
            getGroupByName "A" groups

        groupB =
            getGroupByName "B" groups

        groupC =
            getGroupByName "C" groups

        groupD =
            getGroupByName "D" groups

        quarterFinal1 =
            { homeTeam = getGroupWinner groupA, awayTeam = getGroupRunnerUp groupB, round = QuarterFinal, result = Nothing }

        quarterFinal2 =
            { homeTeam = getGroupWinner groupB, awayTeam = getGroupRunnerUp groupA, round = QuarterFinal, result = Nothing }

        quarterFinal3 =
            { homeTeam = getGroupWinner groupC, awayTeam = getGroupRunnerUp groupD, round = QuarterFinal, result = Nothing }

        quarterFinal4 =
            { homeTeam = getGroupWinner groupD, awayTeam = getGroupRunnerUp groupC, round = QuarterFinal, result = Nothing }
    in
    [ quarterFinal1, quarterFinal2, quarterFinal3, quarterFinal4 ]

getSemiFinalFixtures : List Fixture -> List Fixture
getSemiFinalFixtures quarterFinalResults =
    let
        winners = List.map getFixtureWinner quarterFinalResults
        firstPair = List.take 2 winners
        secondPair = List.take 2 (List.drop 2 winners)
        firstSemiFinalHomeTeam = getTeamFromList firstPair
        firstSemiFinalAwayTeam = getTeamFromList(List.drop 1 firstPair)
        secondSemiFinalHomeTeam = getTeamFromList secondPair
        secondSemiFinalAwayTeam = getTeamFromList (List.drop 1 secondPair)
        firstSemiFinal = { homeTeam = firstSemiFinalHomeTeam, awayTeam = firstSemiFinalAwayTeam, round = SemiFinal, result = Nothing }
        secondSemiFinal = { homeTeam = secondSemiFinalHomeTeam, awayTeam = secondSemiFinalAwayTeam, round = SemiFinal, result = Nothing }
    in
    [firstSemiFinal, secondSemiFinal]



getFinalFixture : List Fixture -> Fixture
getFinalFixture semiFinalResults =
    let
        winners = List.map getFixtureWinner semiFinalResults
        firstTeam = getTeamFromList winners
        secondTeam = getTeamFromList (List.drop 1 winners)
    in
    { homeTeam = firstTeam, awayTeam = secondTeam, round = Final, result = Nothing }

getTeamFromList: List String -> String
getTeamFromList teams =
    let
        maybeTeam = List.head (List.take 1 teams)
    in
    case maybeTeam of
        Just team -> team
        Nothing -> ""

getFixtureWinner : Fixture -> String
getFixtureWinner fixture =
    case fixture.result of
        Just HomeWin -> fixture.homeTeam
        Just Draw -> fixture.homeTeam
        Just HomeLoss -> fixture.awayTeam
        Nothing -> ""


getGroupByName : String -> List Group -> Group
getGroupByName name groups =
    let
        filteredGroups =
            List.filter (groupNameMatch name) groups

        maybeGroup =
            List.head filteredGroups
    in
    case maybeGroup of
        Just group ->
            group

        Nothing ->
            { name = "", teams = [], fixtures = [] }


getGroupWinner : Group -> String
getGroupWinner group =
    let
        maybeWinner =
            List.head (teamsOrderedByPoints group)
    in
    case maybeWinner of
        Just winner ->
            winner.name

        Nothing ->
            ""


getGroupRunnerUp : Group -> String
getGroupRunnerUp group =
    let
        maybeTeams =
            List.tail (teamsOrderedByPoints group)

        teams =
            case maybeTeams of
                Just justTeams ->
                    justTeams

                Nothing ->
                    []

        maybeRunnerUp =
            List.head teams
    in
    case maybeRunnerUp of
        Just runnerUp ->
            runnerUp.name

        Nothing ->
            ""


playRound : Model -> Model
playRound model =
    let
        updatedGroups =
            case model.lastPlayedRound of
                Nothing ->
                    playGroupsRound model.groups model.randomFloat

                Just Round1 ->
                    playGroupsRound model.groups model.randomFloat

                Just Round2 ->
                    playGroupsRound model.groups model.randomFloat

                Just Round3 ->
                    model.groups

                Just QuarterFinal ->
                    model.groups

                Just SemiFinal ->
                    model.groups

                Just Final ->
                    model.groups

        justPlayedRound =
            case model.lastPlayedRound of
                Nothing ->
                    Just Round1

                Just Round1 ->
                    Just Round2

                Just Round2 ->
                    Just Round3

                Just Round3 ->
                    Just QuarterFinal

                Just QuarterFinal ->
                    Just SemiFinal

                Just SemiFinal ->
                    Just Final

                Just Final ->
                    Nothing

        updatedQuarterFinals =
            case justPlayedRound of
                Just Round3 -> Just (getQuarterFinalFixtures model.groups)
                Just QuarterFinal -> Just (Tuple.first (playFixtures model.randomFloat model.quarterFinalFixtures (getAllTeams model.groups)))
                Just SemiFinal -> model.quarterFinalFixtures
                Just Final -> model.quarterFinalFixtures
                _ -> Nothing

        quarterFinalFixtures =
            case updatedQuarterFinals of
                Just fixtures -> fixtures
                Nothing -> []

        updatedSemiFinals =
            case justPlayedRound of
                Just QuarterFinal -> Just (getSemiFinalFixtures quarterFinalFixtures)
                Just SemiFinal -> Just (Tuple.first (playFixtures model.randomFloat model.semiFinalFixtures (getAllTeams model.groups)))
                Just Final -> model.semiFinalFixtures
                _ -> Nothing

        semiFinalFixtures =
            case updatedSemiFinals of
                Just fixtures -> fixtures
                Nothing -> []

        updatedFinal =
            case justPlayedRound of
                Just SemiFinal -> Just (getFinalFixture semiFinalFixtures)
                Just Final ->
                    let
                        x = case model.finalFixture of
                            Just final -> Just (playFixture model.randomFloat (getAllTeams model.groups) final)
                            Nothing -> Nothing
                    in
                    x
                _ -> Nothing
    in
    { model
        | groups = updatedGroups
        , lastPlayedRound = justPlayedRound
        , quarterFinalFixtures = updatedQuarterFinals
        , semiFinalFixtures = updatedSemiFinals
        , finalFixture = updatedFinal
    }

getAllTeams : List Group -> List Team
getAllTeams groups =
    List.concat (List.map getGroupTeams groups)

getGroupTeams: Group -> List Team
getGroupTeams group =
    group.teams


playGroupsRound : List Group -> Float -> List Group
playGroupsRound groups randomFloat =
    List.map (playGroupRound randomFloat) groups


playGroupRound : Float -> Group -> Group
playGroupRound randomFloat group =
    let
        playedFixtures =
            List.filter isPlayedFixture group.fixtures

        unplayedFixtures =
            List.filter isUnplayedFixture group.fixtures

        round1Fixtures =
            List.filter (isGroupRound Round1) unplayedFixtures

        round2Fixtures =
            List.filter (isGroupRound Round2) unplayedFixtures

        round3Fixtures =
            List.filter (isGroupRound Round3) unplayedFixtures

        round1Finished =
            List.isEmpty round1Fixtures

        round2Finished =
            List.isEmpty round2Fixtures

        round3Finished =
            List.isEmpty round3Fixtures
    in
    if not round1Finished then
        let
            result =
                playFixtures randomFloat (Just round1Fixtures) group.teams

            updatedFixtures =
                Tuple.first result ++ round2Fixtures ++ round3Fixtures ++ playedFixtures
        in
        { group | fixtures = updatedFixtures, teams = Tuple.second result }

    else if not round2Finished then
        let
            result =
                playFixtures randomFloat (Just round2Fixtures) group.teams

            updatedFixtures =
                Tuple.first result ++ round3Fixtures ++ playedFixtures
        in
        { group | fixtures = updatedFixtures, teams = Tuple.second result }

    else if not round3Finished then
        let
            result =
                playFixtures randomFloat (Just round3Fixtures) group.teams

            updatedFixtures =
                Tuple.first result ++ playedFixtures
        in
        { group | fixtures = updatedFixtures, teams = Tuple.second result }

    else
        group


playFixtures : Float -> Maybe (List Fixture) -> List Team -> ( List Fixture, List Team )
playFixtures randomFloat maybeFixtures teams =
    let
        updatedFixtures =
            case maybeFixtures of
                Just fixtures -> List.map (playFixture randomFloat teams) fixtures
                Nothing -> []
    in
    ( updatedFixtures, teams )


playFixture : Float -> List Team -> Fixture -> Fixture
playFixture randomFloat teams fixture =
    let
        homeTeamRating =
            getTeamRating (getTeam fixture.homeTeam teams)

        awayTeamRating =
            getTeamRating (getTeam fixture.awayTeam teams)

        result =
            getFixtureResult homeTeamRating awayTeamRating randomFloat
    in
    case result of
        HomeWin ->
            { fixture | result = Just HomeWin }

        Draw ->
            { fixture | result = Just Draw }

        HomeLoss ->
            { fixture | result = Just HomeLoss }


getFixtureResult : Int -> Int -> Float -> GameResult
getFixtureResult homeRating awayRating randomFloat =
    let
        ratingDiff =
            awayRating - homeRating

        x =
            toFloat ratingDiff / 400

        y =
            (10 ^ x) + 1

        probability =
            1 / y

        homeWins =
            randomFloat < probability

        draws =
            randomFloat == probability
    in
    case homeWins of
        True ->
            HomeWin

        False ->
            if draws then
                Draw

            else
                HomeLoss


getTeamRating : Team -> Int
getTeamRating team =
    team.rating


getTeam : String -> List Team -> Team
getTeam name teams =
    let
        filteredTeams =
            List.filter (teamNameMatch name) teams

        maybeTeam =
            List.head filteredTeams
    in
    case maybeTeam of
        Just team ->
            team

        Nothing ->
            { name = "", rating = 0 }


teamNameMatch : String -> Team -> Bool
teamNameMatch name team =
    name == team.name


isGroupRound : Round -> Fixture -> Bool
isGroupRound round fixture =
    round == fixture.round


myButtonModifiers : ButtonModifiers msg
myButtonModifiers =
    { buttonModifiers
        | size = Standard
        , color = Primary
    }


myNavbarModifiers : NavbarModifiers
myNavbarModifiers =
    { navbarModifiers
        | color = Primary
    }


myTabsModifiers : TabsModifiers
myTabsModifiers =
    { tabsModifiers
        | style = Toggle
    }


myTableModifiers : TableModifiers
myTableModifiers =
    { tableModifiers
        | fullWidth = True
    }
