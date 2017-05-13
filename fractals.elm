import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
    render (Line (Point 50 50) (Point 490 50))

type alias Point = { x: Float, y: Float }

type alias Line = { a: Point, b: Point}

render : Line -> Html msg
render ln =
    let
        (a, b) = (ln.a, ln.b)
    in
        let
            x1s = toString( a.x )
            y1s = toString( a.y )
            x2s = toString( b.x )
            y2s = toString( b.y )
        in
        svg [ width "500", height "500", viewBox "0 0 500 500", fill "#DCB35C" ]
            [line [x1 x1s, y1 y1s, x2 x2s, y2 y2s, strokeWidth "3", stroke "black"] []
        ]
