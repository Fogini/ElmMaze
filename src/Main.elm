module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Browser.Navigation as Nav
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Mouse as Mouse
import List exposing (..)
import MyMatrix exposing (..)
import Random
import Tuple exposing (first, second)
import String
import Time
import Time exposing (utc)
import Task



main : Program Float Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta AnimationFrame


size : number
size =
    10


width : number
width =
    size * 2 * wallWidth + wallWidth


height : number
height =
    size * 2 * wallWidth + wallWidth


wallWidth : number
wallWidth =
    25


type alias DrawingPointer =
    { previousMidpoint : Point, lastPoint : Point }


type MazeVertexState
    = Unvisited
    | Visited


type GameState
    = Playing
    | Lost
    | NotPlaying


type alias Model =
    { pending : Array Renderable
    , toDraw : List Renderable
    , maze : List Renderable
    , matrix : Matrix MazeVertexState
    , randomIndexes : List Int
    , drawingPointer : Maybe DrawingPointer
    , color : Color
    , size : Int
    , gameState : GameState
    , canvasClass : String
    , addShapeButton : AddShapeButtonState
    , unixTimestamp : Int 
    , duration: Int
    }


type Msg
    = AnimationFrame Float
    | StartAt ( Float, Float )
    | MoveAt ( Float, Float )
    | EndAt ( Float, Float )
    | Clear
    | Start
    | CreateMaze
    | AddVertex ( Int, Int )
    | CloseModal
    | GenerateRandom
    | NewRandom Int
    | ChangeDropDown
    | ChangeDrawColor Color
    | AddNewEntryClick
    | AddNewEntry Time.Posix
    | Duration Time.Posix
    | DurationClick

type AddShapeButtonState
    = ShowButtonMenu
    | HideButtonMenu

init : Float -> ( Model, Cmd Msg )
init _ =
    ( { pending = Array.empty
      , toDraw = [ shapes [ stroke Color.black ] [ rect ( 0, 0 ) width height ] ]
      , maze = []
      , matrix = MyMatrix.initialize (round (width / wallWidth)) (round (height / wallWidth)) initMatrix
      , randomIndexes = []
      , drawingPointer = Nothing
      , color = Color.black
      , size = 7
      , gameState = NotPlaying
      , canvasClass = ""
      , addShapeButton = HideButtonMenu
      , unixTimestamp =0
      , duration =0
      }
    , Random.generate NewRandom (Random.int 0 2)
    )



initMatrix : Int -> Int -> MazeVertexState
initMatrix x y =
    if x == 0 && y == 1 || x == round (height / wallWidth) - 1 && y == round (height / wallWidth) - 2 then
        Visited

    else
        Unvisited


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            ( model |> flushPendingToDraw, Cmd.none )

        StartAt point ->
            ( initialPoint point model, Cmd.none )

        MoveAt point ->
            case model.gameState of
                Playing ->
                    if get model.matrix ( floor (first point / wallWidth), floor (second point / wallWidth) ) == Just Unvisited then
                        ( { model | gameState = Lost }, Cmd.none )

                    else
                        case model.drawingPointer of
                            Just pointer ->
                                ( drawPoint point pointer model, Cmd.none )

                            Nothing ->
                                ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EndAt point ->
            case model.gameState of
                Playing ->
                    case model.drawingPointer of
                        Just pointer ->
                            ( finalPoint point pointer model, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )
                _ -> 
                    ( model, Cmd.none )

        Clear ->
            -- ( { model | gameState = NotPlaying}, Cmd.none )
            ( model, Nav.reload )

        Start ->
            if length model.maze > 0 then
                -- if maze was created
                ( { model | gameState = Playing }, Cmd.none )

            else
                ( model, Cmd.none )

        CreateMaze ->
            let
                newModel =
                    dfs model 0 ( 1, 1 )
            in
            ( { newModel | maze = buildMaze newModel ( 0, 0 ) }, Cmd.none )

        AddVertex ( x, y ) ->
            ( { model | matrix = set model.matrix ( x, y ) Visited }
            , Cmd.none
            )

        CloseModal ->
            ( { model | gameState = NotPlaying, canvasClass = "red" }
            , Cmd.none
            )

        GenerateRandom ->
            ( model, Random.generate NewRandom (Random.int 0 3) )

        NewRandom newRand ->
            if length model.randomIndexes < (width // wallWidth) * (width // wallWidth) then
                ( { model | randomIndexes = newRand :: model.randomIndexes }, Tuple.second (update GenerateRandom model) )

            else
                ( { model | randomIndexes = newRand :: model.randomIndexes }, Cmd.none )

        ChangeDropDown ->
           ( { model
                | addShapeButton =
                    case model.addShapeButton of
                        HideButtonMenu ->
                            ShowButtonMenu

                        ShowButtonMenu ->
                            HideButtonMenu
            },Cmd.none)
        
        ChangeDrawColor newcolor-> 
            ({model|color=newcolor,addShapeButton=HideButtonMenu},Cmd.none)


        AddNewEntryClick ->
            ( model, Task.perform AddNewEntry Time.now )
        AddNewEntry time ->
            let
                millis = Time.posixToMillis time
            in
            ({ model | unixTimestamp =  millis }
            , Cmd.none)
        
        DurationClick -> 
            ( model, Task.perform Duration Time.now )
        
        Duration time -> 
        
            let 
                millis= Time.posixToMillis time
            in ({model|duration=millis - model.unixTimestamp },Cmd.none)


buildMaze : Model -> ( Int, Int ) -> List Renderable
buildMaze model ( x, y ) =
    if Tuple.second (MyMatrix.size model.matrix) == y then
        []

    else if Tuple.first (MyMatrix.size model.matrix) == x then
        if get model.matrix ( x, y ) == Just Unvisited then
            append [ shapes [ fill Color.black ] [ rect ( toFloat (wallWidth * x), toFloat (wallWidth * y) ) wallWidth wallWidth ] ] (buildMaze model ( 0, y + 1 ))

        else
            buildMaze model ( 0, y + 1 )

    else
    -- if round (width / wallWidth) > x then
    if
        get model.matrix ( x, y ) == Just Unvisited
    then
        append [ shapes [ fill Color.black ] [ rect ( toFloat (wallWidth * x), toFloat (wallWidth * y) ) wallWidth wallWidth ] ] (buildMaze model ( x + 1, y ))

    else
        buildMaze model ( x + 1, y )


dfs : Model -> Int -> ( Int, Int ) -> Model
dfs model i vertex =
    while (first (update (AddVertex vertex) model)) (i + 1) (mewRandVertex model i vertex) vertex


while : Model -> Int -> ( Int, Int ) -> ( Int, Int ) -> Model
while model i nextVertex vertex =
    if nextVertex == ( -1, -1 ) then
        model

    else
        while (dfs (connectVertices model nextVertex vertex) (i + 1) nextVertex) (i + 1) (mewRandVertex model i vertex) vertex


mewRandVertex : Model -> Int -> ( Int, Int ) -> ( Int, Int )
mewRandVertex model i vertex =
    let
        unvisitedNeighbours =
            filter (\v -> get model.matrix v /= Nothing && get model.matrix v /= Just Visited) (neighbours model.matrix vertex)
    in
    let
        newRandom =
            modulo (length unvisitedNeighbours) (Maybe.withDefault 0 (findElem i model.randomIndexes))
    in
    Maybe.withDefault ( -1, -1 ) (findElem newRandom unvisitedNeighbours)


connectVertices : Model -> ( Int, Int ) -> ( Int, Int ) -> Model
connectVertices model ( x1, y1 ) ( x2, y2 ) =
    if get model.matrix ( x1, y1 ) == Just Visited then
        model

    else
        first (update (AddVertex ( (x1 + x2) // 2, (y1 + y2) // 2 )) model)


modulo : Int -> Int -> Int
modulo a b =
    if a <= 0 || b == 0 then
        0

    else
        modBy a b


findElem : Int -> List a -> Maybe a
findElem i xs =
    List.head <| List.drop i xs


flushPendingToDraw : { a | pending : Array b, toDraw : List b } -> { a | pending : Array b, toDraw : List b }
flushPendingToDraw ({ pending } as model) =
    { model
        | pending = Array.empty
        , toDraw = Array.toList pending
    }


initialPoint : ( a, b ) -> { c | drawingPointer : Maybe { previousMidpoint : ( a, b ), lastPoint : ( a, b ) } } -> { c | drawingPointer : Maybe { previousMidpoint : ( a, b ), lastPoint : ( a, b ) } }
initialPoint point model =
    { model
        | drawingPointer = Just { previousMidpoint = point, lastPoint = point }
    }


drawPoint : ( Float, Float ) -> { a | previousMidpoint : Point, lastPoint : ( Float, Float ) } -> Model -> Model
drawPoint newPoint { previousMidpoint, lastPoint } model =
    let
        newMidPoint =
            controlPoint lastPoint newPoint
    in
    { model
        | drawingPointer = Just { previousMidpoint = newMidPoint, lastPoint = newPoint }
        , pending =
            Array.push (drawLine model [ path previousMidpoint [ quadraticCurveTo lastPoint newMidPoint ] ]) model.pending
    }


finalPoint : Point -> { a | previousMidpoint : Point, lastPoint : Point } -> Model -> Model
finalPoint point { previousMidpoint, lastPoint } ({ pending } as model) =
    { model
        | drawingPointer = Nothing
        , pending =
            Array.push
                (drawLine model
                    [ path previousMidpoint [ quadraticCurveTo lastPoint point ] ]
                )
                pending
    }


controlPoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
controlPoint ( x1, y1 ) ( x2, y2 ) =
    ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )


drawLine : Model -> List Shape -> Renderable
drawLine model line =
    line
        |> shapes
            [ lineCap RoundCap
            , lineJoin RoundJoin
            , lineWidth (toFloat model.size)
            , stroke model.color
            ]

getTime:Int-> Html Msg
getTime zeit= div[][Html.text (String.fromInt (Time.toHour utc (zeit|> Time.millisToPosix ))
  ++ ":" ++
  String.fromInt (Time.toMinute utc (zeit |> Time.millisToPosix ))
  ++ ":" ++
  String.fromInt (Time.toSecond utc (zeit |> Time.millisToPosix ))
  ++ " (UTC)")]

view : Model -> Html Msg
view model =
    div []
        [ viewHeader
        , viewDropdown model
        , viewTime model
        , div [ style "margin" "auto", style "width" (String.fromInt width) ]
            [ viewCanvas model
            , viewModal model
            , viewButtons
            ]
        ]
viewDropdown: Model -> Html Msg
viewDropdown model =
    div[class "container"][case model.addShapeButton of
                    HideButtonMenu ->
                        div
                            [ class "dropdown" ]
                            [ div [ class "dropdown-trigger", onClick ChangeDropDown ]
                                [ button [ class "button is-success" ]
                                    [ span [] [ Html.text "Zeichenfarbe ändern" ]
                                    , span [ class "icon is-small" ] [ i [ class "fas fa-angle-down" ] [] ]
                                    ]
                                ]
                            ]

                    ShowButtonMenu ->
                        div
                            [ class "dropdown is-active" ]
                            [ div [ class "dropdown-trigger", onClick ChangeDropDown ]
                                [ button [ class "button is-success" ]
                                    [ span [] [ Html.text "Neue Form" ]
                                    , span [ class "icon is-small" ] [ i [ class "fas fa-angle-down" ] [] ]
                                    ]
                                ]
                            , div [ class "dropdown-menu" ]
                                [ div [ class "dropdown-content" ]
                                    [ a
                                        [ class "dropdown-item", class "button is-black"
                                        , onClick (ChangeDrawColor Color.black)
                                        ]
                                        [ Html.text "Schwarz" ],
                                        a
                                        [ class "dropdown-item", class "button is-black"
                                        , onClick (ChangeDrawColor Color.blue)
                                        ]
                                        [ Html.text "Blau" ],
                                        a
                                        [ class "dropdown-item", class "button is-grey"
                                        , onClick (ChangeDrawColor Color.red)
                                        ]
                                        [ Html.text "Rot" ]
                                        ,
                                        a
                                        [ class "dropdown-item",class "button is-green"
                                        , onClick (ChangeDrawColor Color.green)
                                        ]
                                        [ Html.text "Grün" ]
                                        ,
                                        a
                                        [ class "dropdown-item",class "button is-orange"
                                        , onClick (ChangeDrawColor Color.orange)
                                        ]
                                        [ Html.text "Orange" ]
                                        ,
                                        a
                                        [ class "dropdown-item",class "button is-purple"
                                        , onClick (ChangeDrawColor Color.purple)
                                        ]
                                        [ Html.text "Lila" ]
                                    ]
                                ]
                            ]
    ]

viewTime: Model -> Html Msg
viewTime model= div[class "container"][button [ class " button is-success", onClick AddNewEntryClick ] [ Html.text "Start" ],
                      button [ class " button is-success", onClick DurationClick] [ Html.text "Stop" ],
                         getTime model.unixTimestamp,
                         Html.text (String.fromInt model.duration++"ms")]

viewButtons : Html Msg
viewButtons =
    div [ class " buttons " ]
        [ button [ class " button is-success is-rounded mazeButton", onClick Clear ] [ Html.text "Clear" ]
        , button [ class " button is-success is-rounded mazeButton", onClick CreateMaze ] [ Html.text "Create Maze" ]
        , button [ class " button is-success is-rounded mazeButton", onClick Start ] [ Html.text "Start" ]
        ]


viewHeader : Html Msg
viewHeader =
    div [ class " block header" ]
        [ h1 [ class "title" ] [ Html.text "Labyrinth" ]
        , h2 [ class "subtitle" ]
            [ Html.text "Zuerst muss das Labyrinth erstellt und das Spiel gestartet werden. " 
            , Html.text "Dann beginnt das Zeichnen, sobald die Maus das Labyrinth berührt (ohne zu klicken). "
            , Html.text "Wenn eine Wand berührt wird, ist die Runde zu Ende."]     
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    div [ class "block" ]
        [   if model.gameState == Playing || model.gameState == NotPlaying then
                Canvas.toHtml ( width, height )
                    [ class model.canvasClass
                    , Mouse.onEnter (StartAt << .offsetPos)
                    , Mouse.onMove (.offsetPos >> MoveAt)
                    , Mouse.onUp (.offsetPos >> EndAt)
                    ]
                (model.maze ++ model.toDraw)
            else
                div [ style "width" (String.fromInt width), style "height" (String.fromInt height), class "mazeCover"] []
        ]


viewModal : Model -> Html Msg
viewModal model =
    case model.gameState of
        Playing ->
            span [] []

        Lost ->
            div []
                [ div [ class "modal is-active" ]
                    [ div [ class "modal-background" ] []
                    , div [ class "modal-card" ]
                        [ div [] [ modalHeader "Du bist ein Looser" ]
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

        NotPlaying ->
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
