module Theme exposing (..)

import Element exposing (..)


type alias Theme =
    { background : Color
    , text : Color
    , hyperlink : Color
    , primary : Color
    , secondary : Color
    }


light : Theme
light =
    { background = rgb255 222 225 221 -- #dee1dd
    , text = rgb255 40 54 61 -- #28363d
    , hyperlink = rgb255 47 87 93 -- #2f575d
    , primary = rgb255 101 139 111 -- #658b6f
    , secondary = rgb255 109 145 151 -- #6d9197
    }


addOpacity : Float -> Color -> Color
addOpacity opacity =
    toRgb >> (\x -> { x | alpha = opacity }) >> fromRgb
