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
