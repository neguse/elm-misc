module NBody exposing (..)

import AnimationFrame exposing (diffs)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import Time exposing (Time, millisecond, inSeconds)
import Random exposing (Seed)
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes as H exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode exposing (string, map)
import String


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


g : Float
g =
    6.673e-11


except : List a -> Int -> List a
except xs i =
    (List.take i xs) ++ (List.drop (i + 1) xs)


{-| (exceptPair [1,2,3]) = [(1,[2,3]),(2,[1,3]),(3,[1,2])]
-}
exceptPair : List a -> List ( a, List a )
exceptPair xs =
    let
        excepts =
            List.map (except xs) (List.range 0 (List.length xs))
    in
        List.map2 (,) xs excepts


type alias Vec2 =
    { x : Float
    , y : Float
    }


zero : Vec2
zero =
    (Vec2 0 0)


add : Vec2 -> Vec2 -> Vec2
add v1 v2 =
    (Vec2 (v1.x + v2.x) (v1.y + v2.y))


sub : Vec2 -> Vec2 -> Vec2
sub v1 v2 =
    (Vec2 (v1.x - v2.x) (v1.y - v2.y))


scale : Float -> Vec2 -> Vec2
scale f v =
    (Vec2 (v.x * f) (v.y * f))


length : Vec2 -> Float
length v =
    sqrt (v.x * v.x + v.y * v.y)


normalize : Vec2 -> Vec2
normalize v =
    scale (1.0 / (length v)) v


type alias Body =
    { position : Vec2
    , velocity : Vec2
    , mass : Float
    , radius : Float
    }


type alias Model =
    { bodies : List Body
    , scale : Int
    }


sun : Body
sun =
    (Body
        zero
        zero
        1.0e17
        2
    )


randomBody : Seed -> ( Body, Seed )
randomBody s =
    let
        ( d, s2 ) =
            Random.step (Random.float 100 150) s

        ( a, s3 ) =
            Random.step (Random.float 0 (2 * pi)) s2

        ( mass, s4 ) =
            Random.step (Random.float 1.0e3 1.0e4) s3

        ( a2, s5 ) =
            Random.step (Random.int 0 1) s4

        va =
            (a + 0.5 * pi)
                * if a2 == 0 then
                    1
                  else
                    -1

        vf =
            1.0
    in
        ( (Body
            (Vec2 ((cos a) * d) ((sin a) * d))
            (Vec2 ((cos va) * d * vf) ((sin va) * d * vf))
            mass
            1
          )
        , s5
        )


randomBodies : Int -> Seed -> List Body
randomBodies n s =
    if n == 0 then
        []
    else
        let
            ( b, s2 ) =
                randomBody s
        in
            b :: randomBodies (n - 1) s2


init : ( Model, Cmd Msg )
init =
    ( (Model (sun :: (randomBodies 200 (Random.initialSeed 1))) 1)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time
    | ChangeScale String


force : Body -> Body -> Vec2
force b1 b2 =
    let
        dpos =
            (sub b2.position b1.position)

        r =
            length dpos

        mm =
            b1.mass * b2.mass

        f =
            g * mm / (r * r + 1.0e-32)
    in
        scale f (normalize dpos)


forces : Body -> List Body -> Vec2
forces b bodies =
    case bodies of
        b2 :: bs ->
            add (force b b2) (forces b bs)

        [] ->
            zero


step : Float -> List Body -> Body -> Body
step dt bodies body =
    let
        force =
            forces body bodies

        newVelocity =
            add body.velocity (scale (dt / body.mass) force)

        newPosition =
            add body.position (scale dt newVelocity)
    in
        { body
            | position = newPosition
            , velocity = newVelocity
        }


updateBodies : Float -> List Body -> List Body
updateBodies dt bodies =
    let
        pairs =
            exceptPair bodies

        pairStep ( b, bs ) =
            step dt bs b
    in
        List.map pairStep pairs


updateBodiesIter : Float -> Int -> List Body -> List Body
updateBodiesIter dt iter bodies =
    if iter == 0 then
        bodies
    else
        updateBodiesIter dt (iter - 1) (updateBodies dt bodies)


iter =
    1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dtInMsec ->
            ( { model | bodies = updateBodiesIter (dtInMsec * 1.0e-3 / iter) iter model.bodies }, Cmd.none )

        ChangeScale scale ->
            ( { model | scale =  String.toInt scale |> Result.withDefault 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- VIEW


viewBody : Int -> Body -> Svg Msg
viewBody s b =
    let
        pos =
            add (scale (1.0 / (toFloat s)) b.position) (Vec2 300 300)
    in
        circle
            [ S.cx (toString pos.x)
            , S.cy (toString pos.y)
            , S.r (toString b.radius)
            , S.stroke "none"
            , S.fill "#000"
            ]
            []


view : Model -> Html Msg
view model =
    div []
        [ svg [ viewBox "0 0 600 600", S.width "600px" ]
            (List.map (viewBody model.scale) model.bodies)
        , input
            [ H.type_ "range"
            , H.min "1"
            , H.max "10000"
            , value <| toString model.scale
            , onInput ChangeScale
            ]
            []
        ]
