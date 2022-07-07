module Main exposing (..)

import API exposing (Fixture, Group, Team, TeamStats, decodeGroups, decodeTeams, serverEndpoint)
import Browser
import Bulma.CDN exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Debug exposing (..)
import Html exposing (Html, main_, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Random


type Msg
    = FetchGroups
    | FetchGroupsResult (Result Http.Error (List Group))
    | GroupTabClick String
    | SimulateRound
    | GenerateRandomNumber
    | NewRandomNumber Float


type alias Model =
    { ratings : List Team
    , groups : List Group
    , errorMessage : Maybe String
    , activeGroupTab : String
    , randomFloat : Float
    , lastPlayedRound : String
    }


{-| This is the initial state of the system
-}
init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { ratings = []
            , errorMessage = Nothing
            , groups = []
            , activeGroupTab = "A"
            , randomFloat = 0.5
            , lastPlayedRound = ""
            }
    in
    ( model, Cmd.batch [ getGroups ] )


{-| This is the update function that mutates the state of the system
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "Message: " (toString msg)

        _ =
            Debug.log "State: " (toString model)
    in
    case msg of
        FetchGroups ->
            ( model, getGroups )

        FetchGroupsResult (Ok groups) ->
            ( { model | groups = groups }, Cmd.none )

        GroupTabClick groupName ->
            ( { model | activeGroupTab = groupName }, Cmd.none )

        SimulateRound ->
            ( playRound model, Cmd.none )

        GenerateRandomNumber ->
            ( model, Random.generate NewRandomNumber (Random.float 0 1) )

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
                [ tileParent Width8 [ class "notification is-warning" ] []
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
                                [ button myButtonModifiers [ onClick SimulateRound ] [ text "Simulate Tournament" ]
                                ]
                            ]
                        , tileParent Auto
                            []
                            [ tileChild Auto
                                []
                                [ button myButtonModifiers [] [ text "It's Coming Home" ]
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
            tableBody [] (List.map teamRow (listTeamStats group))

        Nothing ->
            tableRow False [] []

listTeamStats: Group -> List TeamStats
listTeamStats group =
    (List.map (getTeamStats group.fixtures) group.teams)

getTeamStats: List Fixture -> Team -> TeamStats
getTeamStats fixtures team =
    let
        playedFixtures = List.filter isPlayedFixture fixtures
        playedHomeFixtures = List.filter (isHomeFixture team.name) playedFixtures
        playedAwayFixtures = List.filter (isAwayFixture team.name) playedFixtures
        wins = getHomeWins playedHomeFixtures + getAwayWins playedAwayFixtures
        draws = getDraws playedHomeFixtures + getAwayWins playedAwayFixtures
        losses = (List.length playedHomeFixtures + List.length playedAwayFixtures) - wins - draws
    in
    { name = team.name
    , wins = wins
    , draws = draws
    , losses = losses
    , position = 0
    }

isPlayedFixture: Fixture -> Bool
isPlayedFixture fixture =
    fixture.result /= "Unplayed"

isUnplayedFixture: Fixture -> Bool
isUnplayedFixture fixture =
    fixture.result == "Unplayed"

isHomeFixture: String -> Fixture -> Bool
isHomeFixture teamName fixture =
    fixture.homeTeam == teamName

isAwayFixture: String -> Fixture -> Bool
isAwayFixture teamName fixture =
    fixture.awayTeam == teamName

getHomeWins: List Fixture -> Int
getHomeWins fixtures =
    List.length (List.filter isHomeWin fixtures)

getAwayWins: List Fixture -> Int
getAwayWins fixtures =
    List.length (List.filter isAwayWin fixtures)

getDraws: List Fixture -> Int
getDraws fixtures =
    List.length (List.filter isDraw fixtures)

isHomeWin: Fixture -> Bool
isHomeWin fixture =
    fixture.result == "HomeWin"

isAwayWin: Fixture -> Bool
isAwayWin fixture =
    fixture.result == "HomeLoss"

isDraw: Fixture -> Bool
isDraw fixture =
    fixture.result == "Draw"

teamRow : TeamStats -> TableRow Msg
teamRow teamStats =
    tableRow False
        []
        [ tableCell [] [text (String.fromInt teamStats.position)]
        , tableCell [] [ text teamStats.name ]
        , tableCell [] [text (String.fromInt (getPlayed teamStats.wins teamStats.draws teamStats.losses))]
        , tableCell [] [text (String.fromInt teamStats.wins)]
        , tableCell [] [text (String.fromInt teamStats.draws)]
        , tableCell [] [text (String.fromInt teamStats.losses)]
        , tableCell [] [text (String.fromInt (getPoints teamStats.wins teamStats.draws)) ]
        ]

getPoints: Int -> Int -> Int
getPoints wins draws =
    (wins * 3) + draws

getPlayed: Int -> Int -> Int -> Int
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

playRound : Model -> Model
playRound model =
    { model |
        groups = playGroupsRound model.groups model.randomFloat
    }

playGroupsRound: List Group -> Float -> List Group
playGroupsRound groups randomFloat =
    List.map (playGroupRound randomFloat) groups

playGroupRound: Float -> Group -> Group
playGroupRound randomFloat group =
    let
        unplayedFixtures = List.filter isUnplayedFixture group.fixtures
        round1Fixtures = List.filter (isGroupRound "Round1") unplayedFixtures
        round2Fixtures = List.filter (isGroupRound "Round2") unplayedFixtures
        round3Fixtures = List.filter (isGroupRound "Round3") unplayedFixtures
        round1Finished = List.isEmpty round1Fixtures
        round2Finished = List.isEmpty round2Fixtures
        round3Finished = List.isEmpty round3Fixtures
    in
    if (not round1Finished) then
        let
            result = playFixtures randomFloat round1Fixtures group.teams
        in { group | fixtures = Tuple.first (result), teams = Tuple.second (result) }
    else if (not round2Finished) then
        let
            result = playFixtures randomFloat round2Fixtures group.teams
        in { group | fixtures = Tuple.first (result), teams = Tuple.second (result) }
    else if (not round3Finished) then
        let
            result = playFixtures  randomFloat round3Fixtures group.teams
        in { group | fixtures = Tuple.first (result), teams = Tuple.second (result) }
    else
        group

playFixtures: Float -> List Fixture -> List Team -> (List Fixture, List Team)
playFixtures randomFloat fixtures teams =
    let
        result = List.map (playFixture randomFloat teams) fixtures
    in
    (result, teams)

playFixture: Float -> List Team -> Fixture -> Fixture
playFixture randomFloat teams fixture =
    let
        homeTeamRating = getTeamRating (getTeam fixture.homeTeam teams)
        awayTeamRating = getTeamRating (getTeam fixture.homeTeam teams)
        probability = probabilityHomeBeatsAway homeTeamRating awayTeamRating
        homeWins = randomFloat < probability
        draws = randomFloat == probability
    in
    case homeWins of
        True -> { fixture | result = "HomeWin"}
        False -> if (draws) then { fixture | result = "Draw"} else { fixture | result = "HomeLoss"}

probabilityHomeBeatsAway: Int -> Int -> Float
probabilityHomeBeatsAway home away =
    let
        ratingDiff = away - home
        x = toFloat ratingDiff / 400
        y = (x ^ 10) + 1
    in
    (1 / y)

getTeamRating: Team -> Int
getTeamRating team =
    team.rating

getTeam: String -> List Team -> Team
getTeam name teams =
    let
      filteredTeams = List.filter (teamNameMatch name) teams
      maybeTeam = List.head filteredTeams
    in
    case maybeTeam of
        Just team -> team
        Nothing -> { name = "", rating = 0 }

teamNameMatch: String -> Team -> Bool
teamNameMatch name team =
    name == team.name

isGroupRound: String -> Fixture -> Bool
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
