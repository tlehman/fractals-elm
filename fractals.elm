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
         ((kochStep interval), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            (flatMap kochStep model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (second*2) Tick

type alias Point = { x: Float, y: Float }

type alias Line = { a: Point, b: Point}

type Msg = Tick Time



-- the Cantor dust is a 0.63-dimensional fractal, it is is compact, totally disconnected and has Lebesgue measure 0
cantorStep : Line -> Model
cantorStep ln =
    let
        scaleFactor = 0.33333
        shiftFactor = 2 * scaleFactor
        left = (scale scaleFactor ln)
        right = (shift (Point shiftFactor 0) left)
    in
        [left, right]

-- the Koch Snowflake
kochStep : Line -> Model
kochStep ln =
    let
        scaleFactor = 0.33333
        shiftFactor = 2 * scaleFactor
        s = scale scaleFactor
        m = shift (Point scaleFactor 0)
        n = shift (Point -scaleFactor 0)
        r = shift (Point shiftFactor 0)
        left = s ln
        innerLeft = m (rotate (-pi/3) left)
        innerRight = r (rotate (pi/3) (n left))
        right = r left
    in
        [left, innerLeft, innerRight, right]

rotate : Float -> Line -> Line
rotate angle ln =
    let
        r = rotateP angle
    in
    { a = r ln.a, b = r ln.b }

-- rotateP treats Point as a vector from (0,0)
rotateP : Float -> Point -> Point
rotateP angle p =
    { x = p.x * cos(angle) - p.y * sin(angle),
      y = p.x * sin(angle) + p.y * cos(angle) }

transform : Line -> Line
transform ln = (shift (Point 50 250) (scale 640 ln))

view : Model -> Html msg
view model =
    svg [ width "640", height "640", viewBox "0 0 640 700", fill "#DCB35C" ]
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
        (line [x1 x1s, y1 y1s, x2 x2s, y2 y2s, strokeWidth "1", stroke "black"]) []

scalePoint : Point -> Float -> Point
scalePoint pt factor =
    { pt | x = pt.x * factor, y = pt.y * factor }

scale : Float -> Line -> Line
scale factor ln =
    { ln | a = (scalePoint ln.a factor),
           b = (scalePoint ln.b factor)}

addp : Point -> Point -> Point
addp p q =
    { p | x = p.x + q.x,
          y = p.y + q.y }


shift : Point -> Line -> Line
shift p ln =
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
