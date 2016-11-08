module Main exposing (..)

import GraphicSVG exposing (..)
import String
import List
import Result
import Maybe
import Array
import Debug exposing (log)


chart t =
    text (toString t) |> filled black


type alias DataGroup =
    List AverageIncomeRow


type alias AverageIncomeRow =
    { identity : Maybe String
    , women : Maybe Int
    , men : Maybe Int
    }


type alias ParsedLine =
    ( Maybe String, Maybe Int )


type alias DataSlices = List ( Int, Int )


group : DataSlices -> List a -> List (List a)
group groupQuantities items =
    let
        group' quantity =
            let
                ( start, end ) =
                    quantity
            in
                Array.fromList items |> Array.slice start end |> Array.toList
    in
        List.map group' groupQuantities


parseLine : String -> ParsedLine
parseLine row =
    let
        elements =
            String.split "," row |> Array.fromList

        toInt str =
            Just (String.toInt str |> Result.withDefault 0)
    in
        ( Array.get 0 elements |> Maybe.map String.trim
        , Array.get 1 elements `Maybe.andThen` toInt
        )


combine : List ParsedLine -> List ParsedLine -> DataGroup
combine a b =
    let
        combine' lineA lineB =
            let
                ( identity, women ) =
                    lineA

                ( _, men ) =
                    lineB
            in
                AverageIncomeRow identity women men
    in
        log "combined" (List.map2 combine' a b)


massage : String -> List String
massage data =
    String.trim data
        |> String.lines


parse' : DataSlices -> String -> List (List ParsedLine)
parse' groupQuantities dataString =
    massage dataString
        |> List.map parseLine
        |> group groupQuantities


parse : DataSlices -> String -> String -> List DataGroup
parse groupQuantities women men =
    List.map2 combine (parse' groupQuantities women) (parse' groupQuantities men)


type Message
    = GameTick Float GetKeyState


main =
    gameApp GameTick
        { model = { t = 0 }
        , view = view
        , update = update
        }


view model =
    let
        t =
            model.t
    in
        collage 1000 500 [ chart t ]


update message model =
    case message of
        GameTick tick _ ->
            { model | t = tick }


data =
    parse [ ( 0, 2 ), ( 2, 4 ), ( 4, 10 ) ] women men


women =
    """
Aboriginal identity,30042
Non-Aboriginal identity,36992
Total visible minority population,33550
Not a visible minority,37902
Non-immigrants,37154
Immigrants,36452
    Before 1981,42842
    1981 to 1990,41336
    1991 to 2000,35161
    2001 to 2009,28461
    """


men =
    """
Aboriginal identity,37910
Non-Aboriginal identity,52238
Total visible minority population,44819
Not a visible minority,54136
Non-immigrants,52268
Immigrants,51317
    Before 1981,60373
    1981 to 1990,56759
    1991 to 2000,48155
    2001 to 2009,41981
      """
