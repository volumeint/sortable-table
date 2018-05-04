module Icons exposing (downArrow, upArrow)

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, height, viewBox, width, xmlSpace)


svgBox : List (Svg.Attribute msg)
svgBox =
    [ height "24"
    , width "24"
    , viewBox "0 0 24 24"
    , xmlSpace "http://www.w3.org/2000/svg"
    ]


upArrow : List (Html.Attribute msg) -> Html msg
upArrow customAttrs =
    svg (Debug.log "custom" customAttrs ++ svgBox)
        [ path [ d "M0 0h24v24H0V0z", fill "none" ] []
        , path [ d "M4 12l1.41 1.41L11 7.83V20h2V7.83l5.58 5.59L20 12l-8-8-8 8z" ] []
        ]


downArrow : List (Html.Attribute msg) -> Html msg
downArrow customAttrs =
    svg (customAttrs ++ svgBox)
        [ path [ d "M0 0h24v24H0V0z", fill "none" ] []
        , path
            [ d "M20 12l-1.41-1.41L13 16.17V4h-2v12.17l-5.58-5.59L4 12l8 8 8-8z" ]
            []
        ]
