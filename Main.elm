module Main exposing (..)

import GraphicSVG exposing (..)
import String
import List
import Result
import Maybe
import Array
import Debug exposing (log)


-- Palette from
-- http://www.mulinblog.com/a-color-palette-optimized-for-data-visualization/


chartBrown =
    rgba 178 145 47 1


chartPurple =
    rgba 178 118 178 1


chartDivider =
    rgba 200 200 200 1


flatten : List (List a) -> List a
flatten list =
    List.foldr (++) [] list


justValues' : Maybe a -> List a -> List a
justValues' item acc =
    case item of
        Nothing ->
            acc

        Just something ->
            acc ++ [ something ]


justValues : List (Maybe a) -> List a
justValues list =
    List.foldl justValues' [] list


labelsFromDataGroup : DataGroup -> List (Maybe (Shape a))
labelsFromDataGroup dataGroup =
    let
        draw row =
            Just (Maybe.withDefault "-" row.identity |> text |> filled black)
    in
        List.map draw dataGroup


animate : Float -> Float -> Float -> Float -> Float
animate delay duration time value =
    let
        t =
            clamp 0 duration (time - delay)
    in
        value * sin (t / duration * (pi / 2))


valuesFromDataGroup : Float -> Maybe Int -> DataGroup -> List (Maybe (Shape a))
valuesFromDataGroup t maxSalary dataGroup =
    let
        draw row =
            let
                women =
                    Maybe.withDefault 0 row.women

                men =
                    Maybe.withDefault 0 row.men

                max =
                    Maybe.withDefault 1 maxSalary

                animate' value =
                    animate 0.25 0.35 t value

                wp =
                    animate' (women / max * 400)

                mp =
                    animate' (men / max * 400)
            in
                Just
                    (group
                        [ women |> toString |> text |> filled black |> move ( wp + 10, 6 )
                        , men |> toString |> text |> filled black |> move ( mp + 10, -4 )
                        , rect wp 10 |> filled chartPurple |> move ( wp / 2, 10 )
                        , rect mp 10 |> filled chartBrown |> move ( mp / 2, 0 )
                        ]
                    )
    in
        List.map draw dataGroup


addGroupSeperators list =
    List.intersperse [ Nothing ] list


drawChart t maxSalary avgSalary dataGroups =
    let
        offset index shape =
            Maybe.map (move ( 0, toFloat (index * -35) )) shape

        group' lists =
            lists
                |> addGroupSeperators
                |> flatten
                |> List.indexedMap offset
                |> justValues
                |> group

        labelsColumn =
            List.map labelsFromDataGroup dataGroups
                |> group'
                |> move ( 0, 220 )

        values =
            List.map (valuesFromDataGroup t maxSalary) dataGroups
                |> group'
                |> move ( 200, 220 )

        legend =
            group
                [ square 10 |> filled chartPurple
                , text "Women" |> filled black |> move (10, -3)
                , square 10 |> filled chartBrown |> move (0, -20)
                , text "Men" |> filled black |> move (10, -24)
                ]

        maxX =
            maxSalary |> Maybe.map toFloat |> Maybe.withDefault 1

        avgX =
            200 + avgSalary / maxX * 400
    in
        group
            [ labelsColumn
            , line ( avgX, -200 ) ( avgX, 250 ) |> outlined (solid 0.5) chartDivider
            , values
            , legend |> move (700, 200)
            ]
            |> move ( -400, 0 )


type alias DataGroup =
    List AverageIncomeRow


type alias AverageIncomeRow =
    { identity : Maybe String
    , women : Maybe Int
    , men : Maybe Int
    }


type alias ParsedLine =
    ( Maybe String, Maybe Int )


type alias DataSlices =
    List ( Int, Int )


groupLines : DataSlices -> List a -> List (List a)
groupLines groupQuantities items =
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
        |> groupLines groupQuantities


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
        collage 1000 500
            [ drawChart t maxSalary avgSalary data |> move (0, -20)
            , text "Average wages and salaries by sociocultural identity in 2010 ($ CAD)"
                |> filled black
                |> scale 1.2
                |> move (-190, 240)
            , text "Source: Statistics Canada, 2011 National Household Survey, Statistics Canada Catalogue no. 99-014-X2011041."
                |> filled black
                |> scale 0.9
                |> move (-220, -240)
            ]


update message model =
    case message of
        GameTick tick _ ->
            { model | t = tick }


data =
    parse [ ( 0, 2 ), ( 2, 4 ), ( 4, 10 ) ] women men


maxSalary =
    flatten data
        -- We know men make more, so we only need their numbers to find
        -- the max salary.
        |>
            List.map .men
        |> justValues
        |> List.maximum


avgSalary =
    let
        salaries =
            flatten data
                |> List.foldl (\item list -> item.women :: item.men :: list) []
                |> justValues

        sum =
            List.sum salaries
    in
        toFloat sum / toFloat (List.length salaries)


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
