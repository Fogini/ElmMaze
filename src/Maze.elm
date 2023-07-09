module Maze exposing (Maze, dfs, initMaze)

import MyMatrix exposing (Matrix, set, get, secondNeighbours, initialize)
import List exposing (filter, length)



type alias Maze =
    { randomIndexes : List Int
    , matrix : Matrix Bool
    , columns : Int
    , rows : Int
    }


initMaze : Int -> Int -> List Int -> Maze
initMaze c r l = 
    { matrix = initialize c r (\_ _ -> False)
            , randomIndexes = l
            , columns = c
            , rows = r
            }      


dfs : Maze -> Maze
dfs maze = 
    func maze 0 (1,1)


func : Maze -> Int -> ( Int, Int ) -> Maze
func maze i vertex =
    while { maze | matrix = set maze.matrix vertex True } (i + 1) (mewRandVertex maze i vertex) vertex


while : Maze -> Int -> ( Int, Int ) -> ( Int, Int ) -> Maze
while maze i nextVertex vertex =
    if nextVertex == ( -1, -1 ) then
        maze
    else
        while (func (connectVertices maze nextVertex vertex) (i + 1) nextVertex) (i + 1) (mewRandVertex maze i vertex) vertex


mewRandVertex : Maze -> Int -> ( Int, Int ) -> ( Int, Int )
mewRandVertex maze i vertex =
    let
        unvisitedNeighbours =
            filter (\v -> get maze.matrix v /= Nothing && get maze.matrix v /= Just True) (secondNeighbours maze.matrix vertex)
    in
    let
        newRandom =
            modulo (length unvisitedNeighbours) (Maybe.withDefault 0 (findElem i maze.randomIndexes))
    in
    Maybe.withDefault ( -1, -1 ) (findElem newRandom unvisitedNeighbours)


connectVertices : Maze -> ( Int, Int ) -> ( Int, Int ) -> Maze
connectVertices maze ( x1, y1 ) ( x2, y2 ) =
    if get maze.matrix ( x1, y1 ) == Just True then
        maze
    else
        { maze | matrix = set maze.matrix ( (x1 + x2) // 2, (y1 + y2) // 2 ) True }


modulo : Int -> Int -> Int
modulo a b =
    if a <= 0 || b == 0 then
        0
    else
        modBy a b


findElem : Int -> List a -> Maybe a
findElem i xs =
    List.head <| List.drop i xs

