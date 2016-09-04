module Main exposing (..)

import Color exposing (Color)
import Html exposing (Html, text)
import Html.App as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown)
import String
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
    { index : Int }


init : ( Model, Cmd Msg )
init =
    ( { index = 0 }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | Click


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | index = (min ((.index model) + 1) (String.length messageBody)) }, Cmd.none )

        Click ->
            ( { model | index = 0 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (millisecond * 100) Tick



-- VIEW


messageBody =
    "ああああああああ"


view : Model -> Html Msg
view model =
    Html.div
        [ onMouseDown Click
        , style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ text (String.left (.index model) messageBody)
        ]
