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
import List exposing (..)
import Maybe exposing (withDefault)
import Maze exposing (Maze, dfs, initMaze)
import MyMatrix exposing (..)
import Random
import String exposing (fromInt, toUpper)
import Svg
import Svg.Attributes as SvgA
import Tuple exposing (first, second)
import Url exposing (Url)
import Json.Decode as Decode
import String exposing (toInt)


-- #######################################
-- Decoder
-- #######################################
keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map Press (Decode.field "key" Decode.string)



settingRespondDecoder : Decode.Decoder Settings
settingRespondDecoder =
    Decode.map5 Settings
        (Decode.field "columns" Decode.int )
        (Decode.field "rows" Decode.int )
        (Decode.field "blockSize" Decode.int ) 
        (Decode.field "KeyBinds" (Decode.dict Decode.string))
        (Decode.field "Objects" (Decode.dict Decode.string))


mazeDecoder : Maze -> Matrix Object
mazeDecoder maze =
    MyMatrix.indexedMap (decodeMaze maze) maze.matrix


decodeMaze : Maze -> Int -> Int -> Bool -> Object
decodeMaze maze x y b =
    if x == maze.columns - 1 && y == 1 then
        Obj "Closed Door" ( x, y )

    else if x == 1 && y == maze.rows - 2 then
        Obj "Key" ( x, y )

    else if x == 0 && y == 1 then
        Obj "Free" ( x, y )

    else if b then
        let
            numberWalls =
                length (filter (\v -> get maze.matrix v == Just False) (firstNeighbours maze.matrix ( x, y )))

            randomInt =
                withDefault 0 (List.head <| List.drop (x * y - 1) maze.randomIndexes)
        in
        if numberWalls == 3 && randomInt >= 6 then
            Obj "Additional Jump" ( x, y )

        else
            Obj "Free" ( x, y )

    else
        Wall ( x, y )


-- #######################################
-- Update
-- #######################################
type Msg
    = CreateMaze
    | GenerateNewMaze
    | GenerateRandom
    | NewRandom Int
    | Press String
    | UpdateSettings SettingMsg String
    | CloseModal
    | LoadedShapes (Result Http.Error Settings)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

type SettingMsg
    = SetColumns 
    | SetRows 
    | SetBlockSize
    | SetKeyBind String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateMaze ->
            if model.game.gameState /= Playing then
                ( { model | game = createNewGame model }, Cmd.none )
            else
                ( model, Cmd.none )

        GenerateNewMaze ->
            ( { model | game = initialGame MazeCover [] MyMatrix.empty }
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
                ( { model | game = gameAction model.game model.settings key }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateSettings settingMsg s ->
            let
                g = model.game 
            in
             ( {model | settings = updateSettings settingMsg model.settings s, game = { g | gameState = MazeCover} }, Cmd.none )


        CloseModal ->
            ( { model | game = initialGame Playing (buildMazeList model.game.mazeMatrix) model.game.mazeMatrix }
            , Cmd.none
            )

        LoadedShapes response ->
            case response of
                Ok shapeResponse ->
                    ( { model | settings = shapeResponse }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )


updateSettings : SettingMsg -> Settings -> String -> Settings
updateSettings settingMsg settings newVal =
    
    case settingMsg of
        SetColumns -> 
            {settings | columns = withDefault 21 (toInt newVal)} 

        SetRows -> 
            {settings | rows = withDefault 21 (toInt newVal)} 
        
        SetBlockSize ->
            {settings | blockSize = withDefault 25 (toInt newVal)} 

        SetKeyBind key -> 
            if String.length newVal == 1 &&  not (member newVal (Dict.values settings.keyBinds)) then
                {settings | keyBinds = Dict.update key (\_ -> Just newVal) settings.keyBinds}
            else 
                settings


-- ##### Create Game #####
createNewGame : Model -> Game
createNewGame model =
    let
        newMaze =
            mazeDecoder (dfs (initMaze model.settings.columns model.settings.rows model.randomIndexes))                    
    in
    initialGame Playing (buildMazeList newMaze) newMaze 


buildMazeList : Matrix Object -> List Object
buildMazeList matrix =
    Obj "Player" ( 0, 1 )
        :: filter
            (\obj ->
                case obj of
                    Obj "Free" _ ->
                        False

                    _ ->
                        True
            )
            (MyMatrix.toList matrix)


-- #######################################
-- Play Game
-- #######################################

gameAction : Game -> Settings -> String -> Game
gameAction game settings value =

    if value == withDefault "" (Dict.get "up" settings.keyBinds) then
            gameResponse {game | lastAction = "up"} ( first game.point, second game.point - 1 )

    else if value == withDefault "" (Dict.get "left" settings.keyBinds) then
            gameResponse {game | lastAction = "left"} ( first game.point - 1, second game.point )

    else if value == withDefault "" (Dict.get "down" settings.keyBinds) then
            gameResponse {game | lastAction = "down"} ( first game.point, second game.point + 1 )

    else if value == withDefault "" (Dict.get "right" settings.keyBinds) then
            gameResponse {game | lastAction = "right"} ( first game.point + 1, second game.point )
    
    else if value == withDefault "" (Dict.get "jump" settings.keyBinds) then
            if game.numberJumps > 0 then jump { game | numberJumps = game.numberJumps - 1 } else game
    else
        game



jump : Game -> Game
jump game =
    case game.lastAction of
        "up" ->
            gameResponse game ( first game.point, second game.point - 2 )

        "left" ->
            gameResponse game ( first game.point - 2, second game.point )

        "down" ->
            gameResponse game ( first game.point, second game.point + 2 )

        "right" ->
            gameResponse game ( first game.point + 2, second game.point )

        _ ->
            game

gameResponse : Game -> ( Int, Int ) -> Game
gameResponse game newPoint =
    let
        obj =
            get game.mazeMatrix newPoint
    in
    case obj of
        Nothing ->
            { game | gameState = Lost }

        Just (Obj "Open Door" _) ->
            { game | gameState = Won, objects = Obj "Player" newPoint :: drop 1 game.objects }

        Just (Obj "Closed Door" _) ->
            game

        Just (Obj "Free" (1,1)) ->         
                { game 
                | point = newPoint
                , objects = Obj "Player" newPoint :: drop 1 game.objects
                , mazeMatrix = set game.mazeMatrix (0,1) (Wall (0,1))
                , walls = Wall (0,1) :: game.walls 
                }

        Just (Wall _) ->
            { game | gameState = Lost, objects = drop 1 game.objects }

        Just (Obj "Free" _) ->
            { game | point = newPoint, objects = Obj "Player" newPoint :: drop 1 game.objects }

        Just (Obj "Key" p) ->
            let
                doorPoint = (first (MyMatrix.size game.mazeMatrix) - 1, 1 )
            in
            
            { game 
            | point = newPoint
            , objects = [Obj "Player" newPoint, Obj "Open Door" doorPoint] 
            ++ drop 1 (removeObject (Obj "Closed Door" doorPoint) (removeObject (Obj "Key" p) game.objects))
            , mazeMatrix = set game.mazeMatrix doorPoint (Obj "Open Door" doorPoint)
            , keyFound = True }

        Just (Obj "Additional Jump" p) ->
            { game 
            | point = newPoint
            , objects = Obj "Player" newPoint :: drop 1 (removeObject (Obj "Additional Jump" p) game.objects)
            , numberJumps = game.numberJumps + 1 }

        _ ->
            game


removeObject : Object -> List Object -> List Object
removeObject target objs =
    filter (\obj -> if obj == target then False else True) objs


-- #######################################
-- View
-- #######################################

view : Model -> Browser.Document Msg
view model =
    { title = "Maze"
    , body =
        [ div []
            [ viewNavbar
            , case model.url.fragment of
                Just "settings" ->
                    viewSettings model

                _ ->
                    viewMaze model
            ]
        ]
    }

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
        {-, div [ class "box" ]
            [ h1 [ class "subtitle" ] [ text "OBJECTS" ]
            , table [ class "table is-striped is-fullwidth is-bordered has-background-primary" ]
                            [ thead [] []
                            , tbody [] 
                                (Dict.foldl setObjectUrls [] model.settings.objUrl)
                            
                            ]
            ]
        -}
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
                    , onInput (UpdateSettings SetColumns)
                    ]
                    []
                , span
                    [ class "tag is-black", style "margin-left" "10"]
                    [ text (fromInt model.settings.columns) ]
                , label [ class "label" ] [ text "ROWS" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , Html.Attributes.min "15"
                    , Html.Attributes.max "33"
                    , Html.Attributes.step "2"
                    , value (fromInt model.settings.rows)
                    , onInput (UpdateSettings SetRows)
                    ]
                    []
                , span
                    [ class "tag is-black", style "margin-left" "10"]
                    [ text (fromInt model.settings.rows) ]
                , label [ class "label" ] [ text "BLOCKSIZE" ]
                , input
                    [ type_ "range"
                    , class "slider"
                    , Html.Attributes.min "15"
                    , Html.Attributes.max "33"
                    , Html.Attributes.step "2"
                    , value (fromInt model.settings.blockSize)
                    , onInput (UpdateSettings SetBlockSize)
                    ]
                    []
                , span
                    [ class "tag is-black", style "margin-left" "10"]
                    [ text (fromInt model.settings.blockSize) ]
                ]
            ]
        ]
    

{-setObjectUrls : String -> String -> List (Html Msg) -> List (Html Msg)
setObjectUrls k v l =
    tr []
        [ td [ style "width" "33%" ]
            [ b [ ] [ Html.text k ]
            ]
        , td []
            [ Svg.svg [ SvgA.width "auto", SvgA.height "30"] [Svg.image [SvgA.width "30", SvgA.height "30", SvgA.xlinkHref v] []] ]
            
        , td []
            [ div [class "field has-addons"] 
                [ div [class "control" ] 
                    [ input [class "input", type_ "url", placeholder "Insert Link", onInput (UpdateSettings (SetObjUrl k))] [] ]
                , div [ class "control" ] 
                    [ span [ class "button is-black"] [ Html.text "Submit"] ]
                ]
            ]
        ]
        :: l
-}


setKeyBinds : String -> String -> List (Html Msg) -> List (Html Msg)
setKeyBinds k v l =
    tr []
        [ td []
            [ span [ class "icon-text" ]
                [ b [] [ Html.text (toUpper k) ]
                ]
            ]
        , td []
            [ b []
                [ Html.text (if v == " " then "Space Bar" else toUpper v)
                ]
            ]
            
        , td []
            [ 
                input [class "input", type_ "text", placeholder "Insert Character", onInput (UpdateSettings (SetKeyBind k))] [] 
                
            ]
        ]
        :: l

viewNavbar :  Html Msg
viewNavbar = 
    nav [ class "navbar is-primary" ]
        [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item", href "#" ]
                [ h1 [ class "title" ] 
                    [ img [ src "https://cdn-icons-png.flaticon.com/128/10473/10473948.png" ] []
                    , span [ ] [ b [style "color" "white"] [ Html.text "Maze" ]]
                    ]
                ]
            ]
        , div [ class "navbar-end" ] 
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "#settings" ] 
                    [ span [ class "icon" ] [ Html.i [ class "fas fa-cog" ] [] ] ] 
                ]   
            ]        
        ]


viewMaze : Model -> Html Msg
viewMaze model =
    div []
        [ viewHeader model
        , div [ style "margin" "auto", style "width" (fromInt (model.settings.columns * model.settings.blockSize)) ]
            [ viewModal model
            , viewSvg model
            , button [ class " button is-success is-rounded mazeButton block", onClick GenerateNewMaze ] [ Html.text "New Maze" ]
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class " block header" ]
        [ h1 [ class "title" ] [ Html.text "Try to get out of the Maze" ]
        , h2 [class "subtitle" ] [ Html.text "But be carefull. The Walls are deadly!" ]
        ]

    
-- ##### Svg ######

viewSvg : Model -> Html Msg
viewSvg model =

    let
        width = model.settings.columns * model.settings.blockSize
        height = model.settings.rows * model.settings.blockSize
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
                        , style "width" (fromInt (round ( toFloat width * 0.6)))
                        -- , style "height" (fromInt (round ( toFloat height * 0.6))) 
                        , class "coverImage"
                        ]
                        []
                    , button [ class "button is-success is-rounded block coverButton", onClick CreateMaze ] [ Html.text "Start" ]
                    , a [ class "button is-info is-rounded block coverButton", href "#settings" ] [Html.text "Keybinds"] 
                    ]
                ]

        _ ->
            div
                [ class "container maze block"
                , style "width" (fromInt width)
                , style "height" (fromInt height)
                ]
                [ figure []
                    [ Svg.svg
                        [ SvgA.width (fromInt width)
                        , SvgA.height (fromInt height)
                        ]
                        (shapesToSVG model)
                    ]
                ]


shapesToSVG : Model -> List (Svg.Svg Msg)
shapesToSVG model =
    List.map
        (\obj ->
            let
                size = model.settings.blockSize
            in
            case obj of
                Obj s ( x, y ) ->
                    Svg.image
                        [ SvgA.x (fromInt (x * size))
                        , SvgA.y (fromInt (y * size))
                        , SvgA.width (fromInt size)
                        , SvgA.height (fromInt size)
                        , SvgA.xlinkHref (withDefault "" (Dict.get s model.settings.objUrl))
                        ]
                        []

                Wall ( x, y ) ->
                    Svg.rect
                        [ SvgA.x (fromInt (x * size))
                        , SvgA.y (fromInt (y * size))
                        , SvgA.width (fromInt size)
                        , SvgA.height (fromInt size)
                        ]
                        []
        )
        (model.game.objects ++ model.game.walls)


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
                            [ table [ class "table" ]
                                [ thead []
                                    [ div [ style "color" "red", style "font-size" "24px" ] [ Html.text "Game Over" ]
                                    ]
                                ]
                            ]
                        , modalFooter []
                        ]
                    ]
                ]

        Won ->
            div []
                [ div [ class "modal is-active" ]
                    [ div [ class "modal-background" ] []
                    , div [ class "modal-card" ]
                        [ div [] [ modalHeader "You're a winner!" ]
                        , section [ class "modal-card-body" ]
                            [ table [ class "table" ]
                                [ thead []
                                    [ div [ style "color" "gold", style "font-size" "24px" ] [ Html.text "Congrats" ]
                                    ]
                                ]
                            ]
                        , modalFooter []
                        ]
                    ]
                ]

        _ ->
            span [] []


modalHeader : String -> Html Msg
modalHeader title =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ] [ Html.text title ]
        , button [ class "delete", ariaLabel "close", onClick CloseModal ] []
        ]


modalFooter : List (Html Msg) -> Html Msg
modalFooter modalButtons =
    div [ class "modal-card-foot" ] modalButtons


ariaLabel : String -> Attribute msg
ariaLabel value =
    Html.Attributes.attribute "aria-label" value

-- #######################################
-- Initialize
-- #######################################
type alias Game =
    { objects : List Object
    , walls : List Object
    , mazeMatrix : Matrix Object
    , lastAction : String
    , numberJumps : Int
    , keyFound : Bool
    , point : ( Int, Int )
    , gameState : GameState
    }


type GameState
    = Playing
    | Lost
    | Won
    | MazeCover


type Object
    = Obj String ( Int, Int )
    | Wall ( Int, Int )

initialGame : GameState -> List Object -> Matrix Object -> Game
initialGame gameState objs maze =
    { walls = filter (\obj -> case obj of
                                    Wall (0,1) ->
                                        False
                                    Wall _ ->
                                        True
                                    _ ->
                                        False
                    ) objs
    , objects = filter (\obj -> case obj of
                                    Obj "Open Door" _ ->
                                        False
                                    Obj _ _ ->
                                        True
                                    _ ->
                                        False
                        ) objs ++ [ Obj "Closed Door" (first (MyMatrix.size maze) - 1, 1) ]
    , mazeMatrix = set (set maze (0,1) (Obj "Free" (0,1))) (first (MyMatrix.size maze) - 1, 1) (Obj "Closed Door" (first (MyMatrix.size maze) - 1, 1)) 
    , lastAction = "right"
    , numberJumps = 1
    , keyFound = False
    , point = ( 0, 1 )
    , gameState = gameState
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
    , key : Nav.Key
    , url : Url
    }

init : Float -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { randomIndexes = []
      , game = initialGame MazeCover [] MyMatrix.empty
      , settings = initialSettings
      , key = key
      , url = url
      }
    , Cmd.batch [ Random.generate NewRandom (Random.int 0 7), getSettings "ressources\\settings.json"] 
    -- Cmd.batch [getD, getSettings "ressources\\settings.json"]
    )


getSettings : String -> Cmd Msg
getSettings url =
    Http.get
        { url = url
        , expect = Http.expectJson LoadedShapes settingRespondDecoder
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
