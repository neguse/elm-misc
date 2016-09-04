module Main exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond, inSeconds)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Time


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( newTime, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (millisecond * 16) Tick



-- VIEW


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 750 750", width "750px" ]
        [ (g [ transform "translate(325,325)" ] (lines model)) ]


lines : Model -> List (Svg msg)
lines t =
    List.map
        (\i ->
            aLine
                ( (barLength (i + 3) t), (barAngle (i + 3) t) )
                ( (barLength i t), (barAngle i t) )
                (barColor i t)
        )
        [0..barNum]


barNum : Int
barNum =
    150


inMySeconds : Time -> Float
inMySeconds t =
    (inSeconds t) * 80.0


barLength : Int -> Time -> Float
barLength i t =
    sin ((toFloat i) * 0.005 * (inMySeconds t)) * 150 + 100


barAngle : Int -> Time -> Float
barAngle i t =
    2 * pi * (0.005 * (inMySeconds t) - (toFloat i) / (toFloat (barNum + 1)))


barColor : Int -> Time -> Color
barColor i t =
    Color.black


aLine : ( Float, Float ) -> ( Float, Float ) -> Color -> Svg msg
aLine ( length, angle ) ( length2, angle2 ) color =
    let
        ( lx1, ly1 ) =
            fromPolar ( length, angle )

        ( lx2, ly2 ) =
            fromPolar ( length2, angle2 )
    in
        Svg.line [ x1 (toString lx1), y1 (toString ly1), x2 (toString lx2), y2 (toString ly2), stroke "#000000" ] []
