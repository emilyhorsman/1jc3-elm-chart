module Main exposing (..)

import GraphicSVG exposing (..)
import Array


chart t =
    text (toString t) |> filled black


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


women =
    """
Aboriginal identity,30042
Non-Aboriginal identity,36992
,
Total visible minority population,33550
Not a visible minority,37902
,
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
,
Total visible minority population,44819
Not a visible minority,54136
,
Non-immigrants,52268
Immigrants,51317
    Before 1981,60373
    1981 to 1990,56759
    1991 to 2000,48155
    2001 to 2009,41981
      """
