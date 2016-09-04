module Popopo exposing (..)

import Color exposing (Color)
import Html exposing (Html, text, br)
import Html.App as Html
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown)
import String
import Time exposing (Time, millisecond, inSeconds)
import List
import Keyboard exposing (KeyCode, downs)


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
    | KeyDown KeyCode
    | Click


currentMessageLength : Model -> Int
currentMessageLength model =
    messageLength (.lineIndex model)


nextCharIndex : Model -> Int
nextCharIndex model =
    min (currentMessageLength model) ((.charIndex model) + 1)


endCharIndex : Model -> Int
endCharIndex model =
    (currentMessageLength model)


hasNextChar : Model -> Bool
hasNextChar model =
    (currentMessageLength model) > (.charIndex model)


nextLineIndex : Model -> Int
nextLineIndex model =
    (.lineIndex model) + 1


hasNextLine : Model -> Bool
hasNextLine model =
    True


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

        KeyDown code ->
            if code /= 32 && code /= 13 then
                ( model, Cmd.none )
            else if hasNextChar model then
                ( (endChar model), Cmd.none )
            else if hasNextLine model then
                ( (nextLine model), Cmd.none )
            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (millisecond * 100) Tick
        , Keyboard.downs KeyDown
        ]



-- VIEW


origMessages =
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


messages : Int -> List String
messages len =
    if (List.length origMessages) > len then
        List.take len origMessages
    else
        List.append origMessages (messages (len - (List.length origMessages)))


messageLength : Int -> Int
messageLength index =
    let
        message =
            List.head (List.drop (index % (List.length origMessages)) origMessages)
    in
        case message of
            Just x ->
                String.length x

            Nothing ->
                0


viewNextLine : Model -> String
viewNextLine model =
    if (not (hasNextChar model)) && (hasNextLine model) && ((round (inSeconds (.now model))) % 2 == 0) then
        "▼"
    else
        ""


viewMessages : Model -> List (Html msg)
viewMessages model =
    let
        repMessages =
            (messages ((.lineIndex model) + 1))
    in
        (List.intersperse (br [] [])
            (List.append
                (List.map text (List.take (.lineIndex model) repMessages))
                (case (List.head (List.drop (.lineIndex model) repMessages)) of
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
            [ ( "position", "fixed" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ (Html.div
            [ style
                [ ( "position", "fixed" )
                , ( "bottom", "0px" )
                ]
            ]
            (viewMessages model)
          )
        ]
