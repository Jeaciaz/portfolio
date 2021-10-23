module Theme exposing (..)

import Element exposing (..)


type alias Theme =
    { background : Color
    , text : Color
    , hyperlink : Color
    , primary : Color
    , secondary : Color
    , white : Color
    }


light : Theme
light =
    { background = rgb255 222 225 221 -- #dee1dd
    , text = rgb255 40 54 61 -- #28363d
    , hyperlink = rgb255 47 87 93 -- #2f575d
    , primary = rgb255 101 139 111 -- #658b6f
    , secondary = rgb255 109 145 151 -- #6d9197
    , white = rgb255 255 255 255 -- #ffffff
    }


addOpacity : Float -> Color -> Color
addOpacity opacity =
    toRgb >> (\x -> { x | alpha = opacity }) >> fromRgb


ifMobileThenElse : a -> a -> DeviceClass -> a
ifMobileThenElse valueThen valueElse deviceClass =
    if List.member deviceClass [ Phone, Tablet ] then
        valueThen

    else
        valueElse


fontSizeH2 : DeviceClass -> Int
fontSizeH2 =
    ifMobileThenElse 24 30


fontSizeH3 : DeviceClass -> Int
fontSizeH3 =
    ifMobileThenElse 20 24


fontSizeText : DeviceClass -> Int
fontSizeText =
    ifMobileThenElse 16 20
