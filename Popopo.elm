module Popopo exposing (..)

import Color exposing (Color)
import Html exposing (Html, text, br)
import Html.App as Html
import Html.Attributes exposing (style, selected, value)
import Html.Events exposing (on, onMouseDown)
import String
import Time exposing (Time, millisecond, inSeconds)
import List
import Keyboard exposing (KeyCode, downs)
import Json.Decode as Json


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { charIndex : Int, lineIndex : Int, now : Time, fontSize : String }


init : ( Model, Cmd Msg )
init =
    ( { charIndex = 0, lineIndex = 0, now = 0, fontSize = "medium" }, Cmd.none )



-- UPDATE


type Msg
    = Tick Time
    | KeyDown KeyCode
    | Click
    | ChangeFontSize String


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

        ChangeFontSize newFontSize ->
            ( { model | fontSize = newFontSize }, Cmd.none )



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


textDiv txt =
    Html.div [] [ text txt ]


viewMessages : Model -> List (Html msg)
viewMessages model =
    let
        repMessages =
            (messages ((.lineIndex model) + 1))
    in
        (List.append
            (List.map textDiv (List.take (.lineIndex model) repMessages))
            (case (List.head (List.drop (.lineIndex model) repMessages)) of
                Just a ->
                    [ textDiv (String.left (.charIndex model) a ++ (viewNextLine model)) ]

                Nothing ->
                    []
            )
        )


fontSizes =
    [ "xx-small"
    , "x-small"
    , "small"
    , "medium"
    , "large"
    , "x-large"
    , "xx-large"
    ]


toOption : String -> String -> Html msg
toOption n v =
    Html.option [ value v, selected (v == n) ] [ text v ]



-- https://github.com/elm-lang/html/issues/23


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" <| Json.map handler <| Json.at [ "target", "value" ] Json.string


view : Model -> Html Msg
view model =
    Html.div
        [ onMouseDown Click
        , style
            [ ( "position", "fixed" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            , ( "font-size", (.fontSize model) )
            ]
        ]
        [ (Html.div
            [ style
                [ ( "position", "fixed" )
                , ( "bottom", "0px" )
                ]
            ]
            (List.append (viewMessages model)
                [ (Html.select
                    [ onChange ChangeFontSize ]
                    (List.map (\x -> toOption (.fontSize model) x) fontSizes)
                  )
                ]
            )
          )
        ]
