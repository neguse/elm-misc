module Popopo exposing (..)

import Color exposing (Color)
import Html exposing (Html, text, br)
import Html.App as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown)
import String
import Time exposing (Time, millisecond, inSeconds)
import List


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { charIndex : Int, lineIndex : Int, now : Time }


init : ( Model, Cmd Msg )
init =
    ( { charIndex = 0, lineIndex = 0, now = 0 }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | Click


currentCharLength : Model -> Int
currentCharLength model =
    case (List.head (List.drop (.lineIndex model) messages)) of
        Just a ->
            String.length a

        Nothing ->
            0


nextCharIndex : Model -> Int
nextCharIndex model =
    min (currentCharLength model) ((.charIndex model) + 1)


endCharIndex : Model -> Int
endCharIndex model =
    (currentCharLength model)


hasNextChar : Model -> Bool
hasNextChar model =
    (currentCharLength model) > (.charIndex model)


nextLineIndex : Model -> Int
nextLineIndex model =
    min ((List.length messages) - 1) ((.lineIndex model) + 1)


hasNextLine : Model -> Bool
hasNextLine model =
    ((List.length messages) - 1) > (.lineIndex model)


nextChar : Model -> Model
nextChar model =
    { model | charIndex = nextCharIndex model }


endChar : Model -> Model
endChar model =
    { model | charIndex = endCharIndex model }


nextLine : Model -> Model
nextLine model =
    { model | charIndex = 0, lineIndex = nextLineIndex model }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                nowModel =
                    { model | now = newTime }
            in
                if hasNextChar nowModel then
                    ( (nextChar nowModel), Cmd.none )
                else
                    ( nowModel, Cmd.none )

        Click ->
            if hasNextChar model then
                ( (endChar model), Cmd.none )
            else if hasNextLine model then
                ( (nextLine model), Cmd.none )
            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (millisecond * 100) Tick



-- VIEW


messages =
    [ "寿限無\x3000寿限無"
    , "五劫の擦り切れ"
    , "海砂利水魚の"
    , "水行末 雲来末 風来末"
    , "食う寝る処に住む処"
    , "藪ら柑子の藪柑子"
    , "パイポ\x3000パイポ"
    , "パイポのシューリンガン"
    , "シューリンガンのグーリンダイ"
    , "グーリンダイのポンポコピーのポンポコナーの"
    , "長久命の長助"
    ]


viewNextLine : Model -> String
viewNextLine model =
    if (not (hasNextChar model)) && (hasNextLine model) && ((round (inSeconds (.now model))) % 2 == 0) then
        "▼"
    else
        ""


viewMessages : Model -> List (Html msg)
viewMessages model =
    (List.intersperse (br [] [])
        (List.append
            (List.map text (List.take (.lineIndex model) messages))
            (case (List.head (List.drop (.lineIndex model) messages)) of
                Just a ->
                    [ text (String.left (.charIndex model) a ++ (viewNextLine model)) ]

                Nothing ->
                    []
            )
        )
    )


view : Model -> Html Msg
view model =
    Html.div
        [ onMouseDown Click
        , style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        (viewMessages model)
