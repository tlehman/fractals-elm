import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List.FlatMap exposing (..)
import Time exposing (Time, second)

-- The model is a list of lines. This works for our fractals of topological dimension at most 1
type alias Model = List Line

init : (Model, Cmd Msg)
init = let
         a = (Point 0 0)
         b = (Point 1 0)
         interval = (Line a b)
       in
         ((cantorStep interval), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            (flatMap cantorStep model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

type alias Point = { x: Float, y: Float }

type alias Line = { a: Point, b: Point}

type Msg = Tick Time

-- the Cantor dust is a 0.63-dimensional fractal, it is is compact, totally disconnected and has Lebesgue measure 0
cantorStep : Line -> Model
cantorStep ln =
    let
        scaleFactor = 0.33333
        shiftFactor = 2 * scaleFactor
        left = (scale ln scaleFactor)
        right = (shift left (Point shiftFactor 0))
    in
        [left, right]

transform : Line -> Line
transform ln = (shift (scale ln 440) (Point 50 50))

view : Model -> Html msg
view model =
    svg [ width "500", height "500", viewBox "0 0 500 500", fill "#DCB35C" ]
      (List.map renderLine model)

renderLine : Line -> Html msg
renderLine ln =
    let
        lnr = transform ln
        (a,b) = (lnr.a, lnr.b)
        x1s = toString( a.x )
        y1s = toString( a.y )
        x2s = toString( b.x )
        y2s = toString( b.y )
    in
        (line [x1 x1s, y1 y1s, x2 x2s, y2 y2s, strokeWidth "3", stroke "black"]) []

scalePoint : Point -> Float -> Point
scalePoint pt factor =
    { pt | x = pt.x * factor, y = pt.y * factor }

scale : Line -> Float -> Line
scale ln factor =
    { ln | a = (scalePoint ln.a factor),
           b = (scalePoint ln.b factor)}

addp : Point -> Point -> Point
addp p q =
    { p | x = p.x + q.x,
          y = p.y + q.y }


shift : Line -> Point -> Line
shift ln p =
    { ln | a = addp ln.a p,
           b = addp ln.b p }

main =
    program
    {
        init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
    }
