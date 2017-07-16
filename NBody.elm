module NBody exposing (..)

import AnimationFrame exposing (diffs)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond, inSeconds)
import Random exposing (Seed)


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
            Random.step (Random.float 0 150) s

        ( a, s3 ) =
            Random.step (Random.float 0 (2 * pi)) s2

        ( mass, s4 ) =
            Random.step (Random.float 100 1000) s3

        va =
            a + 0.5 * pi

        vf =
            1.0
    in
        ( (Body
            (Vec2 ((cos a) * d) ((sin a) * d))
            (Vec2 ((cos va) * d * vf) ((sin va) * d * vf))
            mass
            1
          )
        , s4
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
    ( (Model (sun :: (randomBodies 100 (Random.initialSeed 1))))
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time


force : Body -> Body -> Vec2
force b1 b2 =
    let
        dpos =
            (sub b2.position b1.position)

        r =
            1.0e-30 + length dpos

        mm =
            b1.mass * b2.mass

        f =
            g * mm / (r * r)
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
    10


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dtInMsec ->
            ( { model | bodies = updateBodiesIter (dtInMsec * 1.0e-3 / iter) iter model.bodies }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



-- VIEW


viewBody : Body -> Svg Msg
viewBody b =
    let
        pos =
            add (scale 0.5 b.position) (Vec2 300 300)
    in
        circle
            [ cx (toString pos.x)
            , cy (toString pos.y)
            , r (toString b.radius)
            , stroke "none"
            , fill "#000"
            ]
            []


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 600 600", width "600px" ]
        (List.map viewBody model.bodies)
