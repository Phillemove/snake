module Main exposing (main)

import Area
import Browser
import Browser.Events
import Color
import Data exposing (Data(..))
import Direction
import Html exposing (Html, br, div, text)
import Http exposing (Error(..))
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import List.NonEmpty exposing (NonEmpty(..))
import Point
import Random
import Score
import Snake
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, y)
import Time



-- Constants


fieldSize : Int
fieldSize =
    10


area : Area.Area
area =
    { width = 30, height = 30 }



-- Types


type Msg
    = Key Key
    | Pointgenerate Point.Point
    | Tick
    | Response (Result Http.Error (List Score.Highscore))


type Key
    = Direction Direction.Direction
    | Reload
    | Unknown


type CrashReason
    = Collision
    | OutOfBounds


type Model
    = Crashed CrashReason Game (Data (List Score.Highscore))
    | Running Game Direction.Direction Float


type alias Game =
    { snake : Snake.Snake
    , food : Maybe Point.Point
    , highscore : Int
    }



-- Elm-Architecture init update view subscriptions


init : ( Model, Cmd Msg )
init =
    ( Running
        { snake = Head { x = 0, y = 0 } []
        , food = Nothing
        , highscore = 0
        }
        Direction.Down
        200
    , Random.generate Pointgenerate (Point.generator area)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Running game direction interval ->
            case msg of
                Key key ->
                    case key of
                        Direction dir ->
                            ( Running { game | snake = Snake.move direction game.snake } dir interval, Cmd.none )

                        Reload ->
                            ( model, Cmd.none )

                        Unknown ->
                            ( model, Cmd.none )

                Pointgenerate point ->
                    ( Running { game | food = Just point } direction interval, Cmd.none )

                Tick ->
                    checkFood game direction interval

                Response _ ->
                    ( model, Cmd.none )

        Crashed reason game _ ->
            case msg of
                Key key ->
                    case key of
                        Reload ->
                            init

                        Direction _ ->
                            ( model, Cmd.none )

                        Unknown ->
                            ( model, Cmd.none )

                Response result ->
                    ( Crashed reason game (Data.fromResult result), Cmd.none )

                Tick ->
                    ( model, Cmd.none )

                Pointgenerate _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Running game _ _ ->
            div []
                [ createSvg game
                , br [] []
                , text ("Score: " ++ String.fromInt game.highscore)
                ]

        Crashed reason game score ->
            div []
                [ createSvg game
                , br [] []
                , text ("Game Over: " ++ reasonToString reason)
                , br [] []
                , text ("Your Score: " ++ String.fromInt game.highscore)
                , br [] []
                , text "Scoreboard: "
                , br [] []
                , viewData score
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running _ _ interval ->
            Sub.batch
                [ Time.every interval (\_ -> Tick)
                , Browser.Events.onKeyDown keyDecoder
                ]

        Crashed _ _ _ ->
            Browser.Events.onKeyDown keyDecoder



-- Preparation of data for the view


viewData : Data (List Score.Highscore) -> Html msg
viewData data =
    case data of
        Loading ->
            text "Loading..."

        Failure err ->
            text ("A Failure accured while fetching the scoredata \n" ++ errToString err)

        Success score ->
            div []
                (if score == [] then
                    [ text "No Scores" ]

                 else
                    List.map highscoreToHtml score
                )


errToString : Http.Error -> String
errToString err =
    case err of
        BadUrl _ ->
            "A bad Url was called"

        Timeout ->
            "Connection Timed out"

        NetworkError ->
            "A Network Error occured"

        BadStatus status ->
            "A Bad Status occured " ++ String.fromInt status

        BadBody _ ->
            "A Bad body was returned"


highscoreToHtml : Score.Highscore -> Html msg
highscoreToHtml { player, score } =
    text ("Name : " ++ player ++ " Score: " ++ String.fromInt score ++ "\n")



-- Decoder and HTTP-commands


keyDecoder : Decoder Msg
keyDecoder =
    let
        toDirection string =
            case string of
                "ArrowUp" ->
                    Direction Direction.Up

                "ArrowDown" ->
                    Direction Direction.Down

                "ArrowLeft" ->
                    Direction Direction.Left

                "ArrowRight" ->
                    Direction Direction.Right

                "r" ->
                    Reload

                _ ->
                    Unknown
    in
    Decode.map Key (Decode.map toDirection (Decode.field "key" Decode.string))


getCmd : Cmd Msg
getCmd =
    Http.get
        { url = "http://SomeUrlInLocalNetworks/highscores"
        , expect = Http.expectJson Response Score.highscoreDecoder
        }



-- Check Crash and Food


checkFood : Game -> Direction.Direction -> Float -> ( Model, Cmd Msg )
checkFood game direction interval =
    if Snake.hitFood game.snake game.food then
        ( Running
            { game
                | snake = Snake.move direction (Snake.extend game.snake)
                , highscore = game.highscore + 10
            }
            direction
            (max (interval - 10) 100)
        , Random.generate Pointgenerate (Point.generator area)
        )

    else
        checkCrash game direction interval


checkCrash : Game -> Direction.Direction -> Float -> ( Model, Cmd Msg )
checkCrash game direction interval =
    if Snake.hasCollision (Snake.move direction game.snake) then
        ( Crashed Collision { game | food = Nothing } Loading, getCmd )

    else if Snake.isOutOfBounds (Snake.move direction game.snake) area then
        ( Crashed OutOfBounds { game | food = Nothing } Loading, getCmd )

    else
        ( Running { game | snake = Snake.move direction game.snake } direction interval, Cmd.none )


reasonToString : CrashReason -> String
reasonToString reason =
    case reason of
        Collision ->
            "Collision with Snake"

        OutOfBounds ->
            "Out Of Bounds"



-- Create SVG


createSvg : Game -> Svg msg
createSvg { snake, food } =
    svg [ width (String.fromInt (area.width * fieldSize)), height (String.fromInt (area.height * fieldSize)) ]
        (areaToSvg area
            :: foodToSvg food
            :: List.Extra.reverse (snakeToSvg snake)
        )


foodToSvg : Maybe Point.Point -> Svg msg
foodToSvg food =
    case food of
        Just f ->
            rect
                [ x (String.fromInt (f.x * fieldSize))
                , y (String.fromInt (f.y * fieldSize))
                , width (String.fromInt fieldSize)
                , height (String.fromInt fieldSize)
                , fill (Color.toString Color.Blue)
                ]
                []

        Nothing ->
            text ""


pointToSvg : Int -> Point.Point -> Svg msg
pointToSvg index point =
    rect
        [ x (String.fromInt (point.x * fieldSize))
        , y (String.fromInt (point.y * fieldSize))
        , width (String.fromInt fieldSize)
        , height (String.fromInt fieldSize)
        , fill (Color.toString (Color.HSL (10 * index) 100 40))
        ]
        []


areaToSvg : Area.Area -> Svg msg
areaToSvg svgarea =
    rect
        [ x "0"
        , y "0"
        , width (String.fromInt (svgarea.width * fieldSize))
        , height (String.fromInt (svgarea.height * fieldSize))
        , fill (Color.toString Color.Lightgrey)
        ]
        []


snakeToSvg : Snake.Snake -> List (Svg msg)
snakeToSvg snake =
    List.Extra.indexedMap pointToSvg (List.NonEmpty.toList snake)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
