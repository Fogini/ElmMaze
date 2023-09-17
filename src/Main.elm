module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import List exposing (filter, head, length, member)
import Maybe exposing (withDefault)
import Maze exposing (Maze, dfs, initMaze)
import MyMatrix exposing (Matrix, get, set, firstNeighbours)
import Random
import String exposing (fromFloat, fromInt, left, right, toInt, toUpper)
import Svg
import Svg.Attributes as SvgA
import Task
import Time
import Tuple exposing (first, second)
import Url exposing (Url)



-- #######################################
-- Decoder
-- #######################################


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map Press (Decode.field "key" Decode.string)


settingRespondDecoder : Decode.Decoder Settings
settingRespondDecoder =
    Decode.map5 Settings
        (Decode.field "columns" Decode.int)
        (Decode.field "rows" Decode.int)
        (Decode.field "blockSize" Decode.int)
        (Decode.field "KeyBinds" (Decode.dict Decode.string))
        (Decode.field "Objects" (Decode.dict Decode.string))


instructionRespondDecoder : Decode.Decoder (Dict String String)
instructionRespondDecoder =
    Decode.field "Instructions" (Decode.dict Decode.string)


mazeDecoder : Maze -> Matrix Object
mazeDecoder maze =
    MyMatrix.indexedMap (decodeMaze maze) maze.matrix


decodeMaze : Maze -> Int -> Int -> Bool -> Object
decodeMaze maze x y b =
    if x == maze.columns - 1 && y == 1 then
        ClosedDoor

    else if x == 0 && y == 1 then
        Free

    else if b then
        let
            numberWalls =
                length (filter (\v -> get maze.matrix v == Just False) (firstNeighbours maze.matrix ( x, y )))

            randomInt i =
                withDefault 0 (List.head <| List.drop (x * y - i) maze.randomIndexes)
        in
        if numberWalls == 3 && (maze.columns * maze.rows // 2) < x * y then
            Flash

        else if numberWalls == 2 then
            if randomInt 1 <= 1 && randomInt 4 >= 3 && y < maze.rows // 2 then
                Orc

            else if randomInt 1 <= 1 && randomInt 3 >= 2 && y > maze.rows // 2 then
                Orc

            else if randomInt 1 == 6 && randomInt 3 == 5 && x > maze.columns && y < maze.rows then
                Ghost

            else if randomInt 1 == 4 && randomInt 2 >= 5 then
                Potion

            else if randomInt 1 >= 6 && randomInt 3 <= 1 then
                Dragon

            else
                Free

        else if numberWalls == 1 && y < maze.rows // 2 then
            Dragon

        else if numberWalls == 1 && x < maze.columns // 2 then
            Ghost

        else
            Free

    else
        Wall



-- #######################################
-- Initialize
-- #######################################


type alias Game =
    { mazeMatrix : Matrix Object
    , oldMaze : Matrix Object
    , lastAction : String
    , numberFlashes : Int
    , keyFound : Bool
    , point : ( Int, Int )
    , gameState : GameState
    , gameTime : Int
    , hp : Int
    , maxHP : Int
    , player : Object
    , ghostMode : Bool
    , fire : Maybe ( Int, Int )
    , fireDuration : Int
    , numberFire : Int
    , numberOrcs : Int
    , spear : Maybe Bool
    }


type GameState
    = Playing
    | Lost
    | Won
    | MazeCover


type Object
    = Free
    | Wall
    | OpenDoor
    | ClosedDoor
    | Key
    | Flash
    | Knight
    | Dragon
    | Orc
    | Potion
    | Death
    | Ghost
    | Fire
    | Spear String


initialGame : GameState -> Matrix Object -> Game
initialGame gameState maze =
    let
        columns =
            first (MyMatrix.size maze)

        rows =
            second (MyMatrix.size maze)
    in
    { mazeMatrix = set (set (set (set maze ( 0, 1 ) Free) ( columns - 1, 1 ) ClosedDoor) ( 1, rows - 2 ) Key) ( columns - 2, 1 ) Dragon
    , oldMaze = set (set (set (set maze ( 0, 1 ) Free) ( columns - 1, 1 ) ClosedDoor) ( 1, rows - 2 ) Key) ( columns - 2, 1 ) Dragon
    , lastAction = "move right"
    , numberFlashes = 1
    , keyFound = False
    , point = ( 0, 1 )
    , gameState = gameState
    , gameTime = 0
    , hp = (columns * rows) // 2
    , maxHP = (columns * rows) // 2
    , player = Knight
    , ghostMode = False
    , fire = Nothing
    , fireDuration = 2
    , numberFire = 1
    , numberOrcs = 0
    , spear = Nothing
    }


type alias Settings =
    { columns : Int
    , rows : Int
    , blockSize : Int
    , keyBinds : Dict String String
    , objUrl : Dict String String
    }


initialSettings : Settings
initialSettings =
    { columns = 21
    , rows = 21
    , blockSize = 20
    , keyBinds = Dict.empty
    , objUrl = Dict.empty
    }


type alias Model =
    { randomIndexes : List Int
    , game : Game
    , settings : Settings
    , instructions : Dict String String
    , key : Nav.Key
    , url : Url
    }


init : Float -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { randomIndexes = []
      , game = initialGame MazeCover MyMatrix.empty
      , settings = initialSettings
      , instructions = Dict.empty
      , key = key
      , url = url
      }
    , Cmd.batch
        [ Random.generate NewRandom (Random.int 0 7)
        , getSettings "ressources\\settings.json"
        , getInstructions "ressources\\instructions.json"
        ]
    )



-- #######################################
-- Update
-- #######################################


type Msg
    = CreateGame
    | GenerateNewMaze
    | GenerateRandom
    | NewRandom Int
    | Press String
    | ItemClicked Object
    | UpdateSettings SettingMsg
    | CloseModal
    | Duration Time.Posix
    | LoadedInstructions (Result Http.Error (Dict String String))
    | LoadedSettings (Result Http.Error Settings)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateGame ->
            if model.game.gameState /= Playing then
                ( { model | game = createNewGame model }, Cmd.none )

            else
                ( model, Cmd.none )

        GenerateNewMaze ->
            ( { model | game = initialGame MazeCover MyMatrix.empty }
            , Random.generate NewRandom (Random.int 0 7)
            )

        GenerateRandom ->
            ( model, Random.generate NewRandom (Random.int 0 7) )

        NewRandom newRand ->
            if length model.randomIndexes < model.settings.columns * model.settings.rows then
                ( { model | randomIndexes = newRand :: model.randomIndexes }, Tuple.second (update GenerateRandom model) )

            else
                ( { model | randomIndexes = newRand :: model.randomIndexes }, Cmd.none )

        Press key ->
            if model.game.gameState == Playing then
                -- first move then attack
                ( { model | game = attackResponse (gameAction model.game model.settings key) }, Task.perform Duration Time.now )

            else
                ( model, Cmd.none )

        ItemClicked obj ->
            let
                g =
                    model.game
            in
            case obj of
                Fire ->
                    ( { model
                      | game = attackResponse { g | fire = Just g.point, fireDuration = g.fireDuration + 2, numberFire = 0 }
                      }
                    , Cmd.none
                    )

                Flash ->
                    ( { model | game = flash g }, Cmd.none )

                Spear _ ->
                    ( { model | game = attackResponse { g | spear = Just True } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateSettings settingMsg ->
            updateSettings settingMsg model

        CloseModal ->
            ( { model | game = initialGame Playing model.game.oldMaze }
            , Cmd.none
            )

        Duration time ->
            let
                g =
                    model.game

                millis =
                    Time.posixToMillis time
            in
            if g.gameTime == 0 then
                ( { model | game = { g | gameTime = millis } }, Cmd.none )

            else if model.game.gameState == Won then
                ( { model | game = { g | gameTime = millis - g.gameTime } }, Cmd.none )

            else
                ( model, Cmd.none )

        LoadedSettings response ->
            case response of
                Ok settingResponse ->
                    ( { model | settings = settingResponse }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LoadedInstructions response ->
            case response of
                Ok instructionResponse ->
                    ( { model | instructions = instructionResponse }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                g =
                    model.game
            in
            ( { model | url = url, game = { g | gameState = MazeCover } }, Cmd.none )


type SettingMsg
    = SetColumns String
    | SetRows String
    | SetBlockSize String
    | SetKeyBind String String
    | Reset


updateSettings : SettingMsg -> Model -> ( Model, Cmd Msg )
updateSettings settingMsg model =
    let
        s =
            model.settings
    in
    case settingMsg of
        SetColumns newVal ->
            ( { model | settings = { s | columns = withDefault 21 (toInt newVal) } }, Cmd.none )

        SetRows newVal ->
            ( { model | settings = { s | rows = withDefault 21 (toInt newVal) } }, Cmd.none )

        SetBlockSize newVal ->
            ( { model | settings = { s | blockSize = withDefault 25 (toInt newVal) } }, Cmd.none )

        SetKeyBind key newVal ->
            if String.length newVal == 1 && not (member newVal (Dict.values s.keyBinds)) then
                ( { model | settings = { s | keyBinds = Dict.update key (\_ -> Just newVal) s.keyBinds } }, Cmd.none )

            else
                ( model, Cmd.none )

        Reset ->
            ( model, getSettings "ressources\\settings.json" )



-- ##### Create Game #####


createNewGame : Model -> Game
createNewGame model =
    let
        newMaze =
            mazeDecoder (dfs (initMaze model.settings.columns model.settings.rows model.randomIndexes))
    in
    initialGame Playing newMaze



-- #######################################
-- Play Game
-- #######################################


gameAction : Game -> Settings -> String -> Game
gameAction game settings value =
    if value == withDefault "" (Dict.get "move up" settings.keyBinds) then
        gameResponse { game | lastAction = "move up" } ( first game.point, second game.point - 1 )

    else if value == withDefault "" (Dict.get "move left" settings.keyBinds) then
        gameResponse { game | lastAction = "move left" } ( first game.point - 1, second game.point )

    else if value == withDefault "" (Dict.get "move down" settings.keyBinds) then
        gameResponse { game | lastAction = "move down" } ( first game.point, second game.point + 1 )

    else if value == withDefault "" (Dict.get "move right" settings.keyBinds) then
        gameResponse { game | lastAction = "move right" } ( first game.point + 1, second game.point )

    else if value == withDefault "" (Dict.get "flash" settings.keyBinds) then
        if game.numberFlashes > 0 then
            flash game

        else
            game

    else
        game


flash : Game -> Game
flash game =
    case game.lastAction of
        "move up" ->
            gameResponse { game | numberFlashes = game.numberFlashes - 1 } ( first game.point, second game.point - 2 )

        "move left" ->
            gameResponse { game | numberFlashes = game.numberFlashes - 1 } ( first game.point - 2, second game.point )

        "move down" ->
            gameResponse { game | numberFlashes = game.numberFlashes - 1 } ( first game.point, second game.point + 2 )

        "move right" ->
            gameResponse { game | numberFlashes = game.numberFlashes - 1 } ( first game.point + 2, second game.point )

        _ ->
            game


setFire : Game -> Maybe ( Int, Int )
setFire game =
    let
        orcPoints =
            List.filter (\p -> MyMatrix.get game.mazeMatrix p == Just Orc) (MyMatrix.firstNeighbours game.mazeMatrix game.point)
    in
    if game.fire == Nothing then
        Nothing

    else if length orcPoints >= 1 then
        head orcPoints

    else
        let
            ( x, y ) =
                game.point
        in
        case game.lastAction of
            "move up" ->
                Just ( x, y - 1 )

            "move left" ->
                Just ( x - 1, y )

            "move down" ->
                Just ( x, y + 1 )

            "move right" ->
                Just ( x + 1, y )

            _ ->
                Nothing


throwSpear : Game -> ( ( Int, Int ), String )
throwSpear game =
    let
        ( x, y ) =
            game.point
    in
    case game.lastAction of
        "move up" ->
            ( ( x, y - 2 ), "315" )

        "move left" ->
            ( ( x - 2, y ), "225" )

        "move down" ->
            ( ( x, y + 2 ), "135" )

        "move right" ->
            ( ( x + 2, y ), "45" )

        _ ->
            ( ( 0, 0 ), "0" )


gameResponse : Game -> ( Int, Int ) -> Game
gameResponse game newPoint =
    let
        obj =
            withDefault Wall (get game.mazeMatrix newPoint)
    in
    case obj of
        Wall ->
            if game.ghostMode then
                { game | ghostMode = False, point = newPoint }

            else
                game

        Free ->
            if game.ghostMode then
                { game | point = newPoint }

            else
                checkHP game (game.hp - 1) newPoint Free

        ClosedDoor ->
            game

        OpenDoor ->
            { game | point = newPoint, gameState = Won }

        Key ->
            let
                doorPoint =
                    ( first (MyMatrix.size game.mazeMatrix) - 1, 1 )
            in
            { game
                | point = newPoint
                , mazeMatrix = set (set game.mazeMatrix doorPoint OpenDoor) newPoint Free
                , keyFound = True
            }

        Flash ->
            { game
                | point = newPoint
                , numberFlashes = game.numberFlashes + 1
                , mazeMatrix = set game.mazeMatrix newPoint Free
            }

        Dragon ->
            if game.ghostMode then
                checkHP game game.hp newPoint Dragon

            else
                checkHP { game | numberFire = 1 } (game.hp - game.maxHP // 7) newPoint Death

        Orc ->
            if game.ghostMode then
                checkHP game game.hp newPoint Orc

            else if game.numberOrcs == 4 then
                checkHP { game | numberOrcs = 0, spear = Just False } (game.hp - game.maxHP // 7) newPoint Death

            else
                checkHP { game | numberOrcs = game.numberOrcs + 1 } (game.hp - game.maxHP // 7) newPoint Death

        Potion ->
            { game
                | point = newPoint
                , hp = Basics.min (game.hp + game.maxHP // 13) game.maxHP
                , mazeMatrix = set game.mazeMatrix newPoint Free
                , player =
                    if game.ghostMode then
                        Ghost

                    else
                        Knight
            }

        Death ->
            { game | point = newPoint }

        Ghost ->
            { game
                | point = newPoint
                , player = Ghost
                , ghostMode = True
                , mazeMatrix = set game.mazeMatrix newPoint Free
            }

        Spear _ ->
            { game | point = newPoint, mazeMatrix = set game.mazeMatrix newPoint Free }

        _ ->
            game


checkHP : Game -> Int -> ( Int, Int ) -> Object -> Game
checkHP game newHP newPoint newObj =
    if game.ghostMode then
        { game | point = newPoint, hp = game.hp, ghostMode = False }

    else if newHP < 1 then
        { game | gameState = Lost, hp = 0, point = newPoint }

    else
        { game | point = newPoint, hp = newHP, mazeMatrix = set game.mazeMatrix newPoint newObj, player = Knight }


attackResponse : Game -> Game
attackResponse game =
    if game.spear == Just True then
        let
            ( p, r ) =
                throwSpear game

            obj =
                withDefault Wall (get game.mazeMatrix p)
        in
        case obj of
            Dragon ->
                { game | spear = Nothing, mazeMatrix = set game.mazeMatrix p (Spear r), numberFire = 1 }

            Orc ->
                if game.numberOrcs == 4 then
                    { game | spear = Just False, mazeMatrix = set game.mazeMatrix p (Spear r), numberOrcs = 0 }

                else
                    { game | spear = Nothing, mazeMatrix = set game.mazeMatrix p (Spear r), numberOrcs = game.numberOrcs + 1 }

            Wall ->
                { game | spear = Nothing }

            _ ->
                { game | spear = Nothing, mazeMatrix = set game.mazeMatrix p (Spear r) }

    else
        let
            newFire =
                setFire game
        in
        case newFire of
            Just p ->
                let
                    obj =
                        withDefault Wall (get game.mazeMatrix p)
                in
                case obj of
                    Orc ->
                        if game.numberOrcs == 4 then
                            { game
                                | mazeMatrix = set game.mazeMatrix p Death
                                , fire = newFire
                                , numberOrcs = 0
                                , spear = Just False
                            }

                        else
                            { game
                                | mazeMatrix = set game.mazeMatrix p Death
                                , fire = newFire
                                , numberOrcs = game.numberOrcs + 1
                            }

                    _ ->
                        { game
                            | fireDuration = Basics.max (game.fireDuration - 1) 0
                            , fire =
                                if game.fireDuration == 0 then
                                    Nothing

                                else
                                    setFire game
                        }

            Nothing ->
                game



-- #######################################
-- View
-- #######################################


view : Model -> Browser.Document Msg
view model =
    case model.url.fragment of
        Just "settings" ->
            { title = "Settings", body = [ div [] [ viewNavbar, viewSettings model ] ] }

        Just "instructions" ->
            { title = "Instructions", body = [ div [] [ viewNavbar, viewInstructions model ] ] }

        _ ->
            { title = "Maze", body = [ div [] [ viewNavbar, viewMaze model ] ] }


viewNavbar : Html Msg
viewNavbar =
    nav [ class "navbar is-primary" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "#" ]
                [ h1 [ class "title" ]
                    [ img [ src "https://cdn-icons-png.flaticon.com/128/10473/10473948.png" ] []
                    , span [ style "color" "white" ] [ b [] [ text "Maze" ] ]
                    ]
                ]
            ]
        , div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "#instructions" ] [ h1 [ class "title" ] [ b [ style "color" "white" ] [ text "Instructions" ] ] ]
            ]
        , div [ class "navbar-end" ]
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "#settings" ]
                    [ span [ class "icon" ] [ Html.i [ class "fas fa-cog" ] [] ] ]
                ]
            ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    div []
        [ div [ class "box" ]
            [ h1 [ class "subtitle" ] [ text "MOVEMENT" ]
            , table [ class "table is-striped is-fullwidth is-bordered has-background-primary" ]
                [ thead [] []
                , tbody []
                    (Dict.foldl setKeyBinds [] model.settings.keyBinds)
                ]
            ]
        , div [ class "box" ]
            [ h1 [ class "subtitle" ] [ text "MAZE MEASURES" ]
            , div [ class "field" ]
                [ label [ class "label" ] [ text "COLUMNS" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , Html.Attributes.min "15"
                    , Html.Attributes.max "33"
                    , Html.Attributes.step "2"
                    , value (fromInt model.settings.columns)
                    , onInput (\new -> UpdateSettings (SetColumns new))
                    ]
                    []
                , span
                    [ class "tag is-black", style "margin-left" "10" ]
                    [ text (fromInt model.settings.columns) ]
                , label [ class "label" ] [ text "ROWS" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , Html.Attributes.min "15"
                    , Html.Attributes.max "33"
                    , Html.Attributes.step "2"
                    , value (fromInt model.settings.rows)
                    , onInput (\new -> UpdateSettings (SetRows new))
                    ]
                    []
                , span
                    [ class "tag is-black", style "margin-left" "10" ]
                    [ text (fromInt model.settings.rows) ]
                , label [ class "label" ] [ text "BLOCKSIZE" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , Html.Attributes.min "15"
                    , Html.Attributes.max "33"
                    , Html.Attributes.step "1"
                    , value (fromInt model.settings.blockSize)
                    , onInput (\new -> UpdateSettings (SetBlockSize new))
                    ]
                    []
                , span
                    [ class "tag is-black", style "margin-left" "10" ]
                    [ text (fromInt model.settings.blockSize) ]
                ]
            ]
        , div [ class "box" ]
            [ button [ class "block button is-danger", onClick (UpdateSettings Reset) ] [ text "Reset Settings" ] ]
        ]


setKeyBinds : String -> String -> List (Html Msg) -> List (Html Msg)
setKeyBinds k v l =
    tr []
        [ td []
            [ span [ class "icon-text" ]
                [ b [] [ text (toUpper k) ]
                ]
            ]
        , td []
            [ b []
                [ text
                    (if v == " " then
                        "Space Bar"

                     else
                        toUpper v
                    )
                ]
            ]
        , td []
            [ input [ class "input", type_ "text", placeholder "Insert Character", onInput (\new -> UpdateSettings (SetKeyBind k new)) ] []
            ]
        ]
        :: l


viewInstructions : Model -> Html Msg
viewInstructions model =
    div []
        [ div [ class "box" ]
            [ h1 [ class "subtitle" ] [ text "Boosts" ]
            , div [ class "columns" ]
                [ viewObjectCard model Ghost
                , viewObjectCard model Potion
                , viewObjectCard model Flash
                , viewObjectCard model Death
                ]
            ]
        , div [ class "box" ]
            [ h1 [ class "subtitle" ] [ text "General" ]
            , div [ class "columns" ]
                [ viewObjectCard model OpenDoor
                , viewObjectCard model ClosedDoor
                , viewObjectCard model Knight
                , viewObjectCard model Key
                ]
            ]
        , div [ class "box" ]
            [ h1 [ class "subtitle" ] [ text "Attacks and Enemies" ]
            , div [ class "columns" ]
                [ viewObjectCard model Fire
                , viewObjectCard model (Spear "")
                , viewObjectCard model Dragon
                , viewObjectCard model Orc
                ]
            ]
        , div [ class "box" ]
            [ h1 [ class "subtitle" ] [ text "Settings" ]
            , a [ class "block button is-info", href "#settings" ] [ text "Adjust Keybinds and Maze measures" ]
            ]
        ]


viewObjectCard : Model -> Object -> Html Msg
viewObjectCard model obj =
    div [ class "column card" ]
        [ div [ class "card-content" ]
            [ div [ class "media" ]
                [ div [ class "media-left" ]
                    [ figure [ class "image is-48x48" ]
                        [ img [ src (withDefault "" (Dict.get (objectToString obj) model.settings.objUrl)) ] [] ]
                    ]
                , div [ class "media-content" ]
                    [ p [ class "title is-4 " ] [ text (objectToString obj) ] ]
                ]
            , div [ class "content" ]
                [ text (withDefault "" (Dict.get (objectToString obj) model.instructions)) ]
            ]
        ]


viewMaze : Model -> Html Msg
viewMaze model =
    div []
        [ viewHeader model
        , div [ style "margin" "auto", style "width" (fromInt (model.settings.columns * model.settings.blockSize)) ]
            [ viewModal model
            , viewSvg model
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class " block header" ]
        [ h1 [ class "title" ] [ text "Try to survive the Maze" ]
        ]



-- ##### Svg ######


viewSvg : Model -> Html Msg
viewSvg model =
    let
        width =
            model.settings.columns * model.settings.blockSize

        height =
            model.settings.rows * model.settings.blockSize
    in
    case model.game.gameState of
        MazeCover ->
            div
                [ class "block mazeCover maze"
                , style "width" (fromInt width)
                , style "height" (fromInt height)
                ]
                [ div [ class "keyBinds" ]
                    [ img
                        [ src "https://st2.depositphotos.com/1001911/6587/v/450/depositphotos_65874439-stock-illustration-outlined-cartoon-light-bulb.jpg"
                        , style "width" (fromInt (round (toFloat width * 0.6)))
                        , style "height" (fromInt (round (toFloat height * 0.7)))
                        , class "coverImage"
                        ]
                        []
                    , button [ class "button is-success is-rounded block coverButton", onClick CreateGame ] [ text "Start" ]
                    , a [ class "button is-info is-rounded block coverButton", href "#instructions" ] [ text "Instructions" ]
                    ]
                ]

        _ ->
            div [ class "columns" ]
                [ div [ style "width" (fromInt width), class "column block" ]
                    [ div
                        [ class "container maze block"
                        , style "width" (fromInt width)
                        , style "height" (fromInt height)
                        ]
                        [ figure []
                            [ Svg.svg [ SvgA.width (fromInt width), SvgA.height (fromInt height) ]
                                ([ objectToSvg model (Just model.game.point) model.game.player, objectToSvg model model.game.fire Fire ]
                                    ++ objectMatrixToSVG model
                                )
                            ]
                        , viewHealthBar model
                        , button [ class " button is-success is-rounded mazeButton block", onClick GenerateNewMaze ] [ text "New Maze" ]
                        ]
                    ]
                , viewItems model
                ]


objectMatrixToSVG : Model -> List (Svg.Svg Msg)
objectMatrixToSVG model =
    MyMatrix.toList
        (MyMatrix.indexedMap
            (\x y obj ->
                case obj of
                    Wall ->
                        Svg.rect
                            [ SvgA.x (fromInt (x * model.settings.blockSize))
                            , SvgA.y (fromInt (y * model.settings.blockSize))
                            , SvgA.width (fromInt model.settings.blockSize)
                            , SvgA.height (fromInt model.settings.blockSize)
                            , SvgA.style "background-color: black"
                            ]
                            []

                    Free ->
                        Svg.svg [] []

                    _ ->
                        objectToSvg model (Just ( x, y )) obj
            )
            model.game.mazeMatrix
        )


objectToSvg : Model -> Maybe ( Int, Int ) -> Object -> Svg.Svg Msg
objectToSvg model p obj =
    let
        s =
            model.settings
    in
    case p of
        Nothing ->
            Svg.svg [] []

        Just ( x, y ) ->
            case obj of
                Spear r ->
                    Svg.image
                        [ SvgA.x (fromInt (x * s.blockSize))
                        , SvgA.y (fromInt (y * s.blockSize))
                        , SvgA.width (fromInt s.blockSize)
                        , SvgA.height (fromInt s.blockSize)
                        , SvgA.transform
                            ("rotate("
                                ++ r
                                ++ ","
                                ++ fromFloat (toFloat (x * s.blockSize) + toFloat s.blockSize / 2)
                                ++ ","
                                ++ fromFloat (toFloat (y * s.blockSize) + toFloat s.blockSize / 2)
                                ++ ")"
                            )
                        , SvgA.xlinkHref (withDefault "" (Dict.get (objectToString obj) model.settings.objUrl))
                        ]
                        []

                _ ->
                    Svg.image
                        [ SvgA.x (fromInt (x * model.settings.blockSize))
                        , SvgA.y (fromInt (y * model.settings.blockSize))
                        , SvgA.width (fromInt model.settings.blockSize)
                        , SvgA.height (fromInt model.settings.blockSize)
                        , SvgA.xlinkHref (withDefault "" (Dict.get (objectToString obj) model.settings.objUrl))
                        ]
                        []


objectToString : Object -> String
objectToString obj =
    case obj of
        Knight ->
            "Knight"

        OpenDoor ->
            "OpenDoor"

        ClosedDoor ->
            "ClosedDoor"

        Key ->
            "Key"

        Flash ->
            "Flash"

        Dragon ->
            "Dragon"

        Orc ->
            "Orc"

        Potion ->
            "Potion"

        Death ->
            "Death"

        Ghost ->
            "Ghost"

        Fire ->
            "Fire"

        Spear _ ->
            "Spear"

        Free ->
            ""

        Wall ->
            ""


viewHealthBar : Model -> Html Msg
viewHealthBar model =
    let
        maxHP =
            model.settings.rows * model.settings.columns // 2

        hp =
            model.game.hp

        color =
            if hp >= maxHP // 2 then
                "is-success"

            else if hp >= maxHP // 4 then
                "is-warning"

            else
                "is-danger"
    in
    div []
        [ progress [ class ("block progress hpBar " ++ color), value (fromInt hp), Html.Attributes.max (fromInt maxHP) ] []

        -- , span [ class "tag is-black", style "margin-left" "10"] [text (fromInt hp) ]
        ]


viewItems : Model -> Html Msg
viewItems model =
    div
        [ class "column"
        , style "height" (fromInt (model.settings.rows * model.settings.blockSize))
        , style "margin" "13 0 0 30"
        , style "border" "4px solid hsl(141, 71%, 48%)"
        ]
        ([ if model.game.keyFound then
            div [ style "font-size" "18" ] [ b [] [ text "Key" ] ]

           else
            span [] []
         , if model.game.keyFound then
            objectToImg model Key

           else
            span [] []
         , if model.game.ghostMode || model.game.numberFlashes > 0 then
            div [ style "font-size" "18" ] [ b [] [ text "Boosts" ] ]

           else
            span [] []
         , if model.game.ghostMode then
            objectToImg model Ghost

           else
            span [] []
         ]
            ++ List.repeat model.game.numberFlashes (objectToImg model Flash)
            ++ [ if model.game.spear /= Nothing || model.game.numberFire > 0 then
                    div [ style "font-size" "18" ] [ b [] [ text "Attacks" ] ]

                 else
                    span [] []
               , if model.game.spear /= Nothing then
                    objectToImg model (Spear "0")

                 else
                    span [] []
               ]
            ++ List.repeat model.game.numberFire (objectToImg model Fire)
            ++ (if model.game.numberOrcs > 0 then
                    [ div [ style "font-size" "18" ] [ b [] [ text "Orcs" ] ]
                    , objectToImg model Orc
                    , div [ style "font-size" "14", style "width" "35%", style "margin" "auto" ] [ b [] [ text (fromInt model.game.numberOrcs) ] ]
                    ]

                else
                    [ span [] [] ]
               )
        )


objectToImg : Model -> Object -> Html Msg
objectToImg model obj =
    img
        [ width (model.settings.blockSize * 2)
        , height (model.settings.blockSize * 2)
        , src (withDefault "" (Dict.get (objectToString obj) model.settings.objUrl))
        , class "itemImg"
        , onClick (ItemClicked obj)
        ]
        []



-- ##### Modal ######


viewModal : Model -> Html Msg
viewModal model =
    case model.game.gameState of
        Lost ->
            div []
                [ div [ class "modal is-active" ]
                    [ div [ class "modal-background" ] []
                    , div [ class "modal-card" ]
                        [ div [] [ modalHeader "You're a Looser!" ]
                        , section [ class "modal-card-body" ]
                            [ div [ style "color" "red", style "font-size" "24px" ] [ text "Game Over" ] ]
                        , div [ class "modal-card-foot" ]
                            [ a [ class "button is-info is-rounded", href "#settings" ]
                                [ b [] [ text "Adjust Settings" ] ]
                            ]
                        ]
                    ]
                ]

        Won ->
            let
                millis =
                    String.fromInt model.game.gameTime
            in
            div []
                [ div [ class "modal is-active" ]
                    [ div [ class "modal-background" ] []
                    , div [ class "modal-card" ]
                        [ div [] [ modalHeader "You're a winner!" ]
                        , section [ class "modal-card-body" ]
                            [ div [ style "color" "gold", style "font-size" "24px" ] [ text "Congrats" ]
                            , div [ style "font-size" "20px" ]
                                [ text "Completed in "
                                , text (left (String.length millis - 3) millis ++ "." ++ right 3 millis ++ " seconds")
                                , text (" with " ++ fromInt model.game.hp ++ " HP left.")
                                ]
                            ]
                        , div [ class "modal-card-foot" ] []
                        ]
                    ]
                ]

        _ ->
            span [] []


modalHeader : String -> Html Msg
modalHeader title =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ] [ text title ]
        , button [ class "delete", ariaLabel "close", onClick CloseModal ] []
        ]


ariaLabel : String -> Attribute msg
ariaLabel value =
    Html.Attributes.attribute "aria-label" value


getSettings : String -> Cmd Msg
getSettings url =
    Http.get
        { url = url
        , expect = Http.expectJson LoadedSettings settingRespondDecoder
        }


getInstructions : String -> Cmd Msg
getInstructions url =
    Http.get
        { url = url
        , expect = Http.expectJson LoadedInstructions instructionRespondDecoder
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyPress keyDecoder


main : Program Float Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
