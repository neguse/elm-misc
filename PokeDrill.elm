module PokeDrill exposing (..)

import Html exposing (Html, program, div, table, th, td, tr, text)
import Html.Attributes exposing (style)
import Dict exposing (Dict)


-- MODEL


type PokeType
    = Normal
    | Fight
    | Flying
    | Poison
    | Ground
    | Rock
    | Bug
    | Ghost
    | Steel
    | Fire
    | Water
    | Grass
    | Electr
    | Psychc
    | Ice
    | Dragon
    | Dark
    | Fairy


pokeTypeJa : PokeType -> String
pokeTypeJa t =
    case t of
        Normal ->
            "ノーマル"

        Fight ->
            "かくとう"

        Flying ->
            "ひこう"

        Poison ->
            "どく"

        Ground ->
            "じめん"

        Rock ->
            "いわ"

        Bug ->
            "むし"

        Ghost ->
            "ゴースト"

        Steel ->
            "はがね"

        Fire ->
            "ほのお"

        Water ->
            "みず"

        Grass ->
            "くさ"

        Electr ->
            "でんき"

        Psychc ->
            "エスパー"

        Ice ->
            "こおり"

        Dragon ->
            "ドラゴン"

        Dark ->
            "あく"

        Fairy ->
            "フェアリー"


pokeTypes : List PokeType
pokeTypes =
    [ Normal
    , Fight
    , Flying
    , Poison
    , Ground
    , Rock
    , Bug
    , Ghost
    , Steel
    , Fire
    , Water
    , Grass
    , Electr
    , Psychc
    , Ice
    , Dragon
    , Dark
    , Fairy
    ]


type Effectiveness
    = Effective
    | SuperEffective
    | NotVeryEffective
    | Ineffective


effectivenessStr : Effectiveness -> String
effectivenessStr e =
    case e of
        Effective ->
            "そのまま"

        SuperEffective ->
            "ばつぐん"

        NotVeryEffective ->
            "いまひとつ"

        Ineffective ->
            "こうかがない"


type alias ChartEntry =
    { attackType : PokeType, blockType : PokeType, effectiveness : Effectiveness }


typeChart : List ChartEntry
typeChart =
    [ -- Normal
      { attackType = Normal, blockType = Rock, effectiveness = NotVeryEffective }
    , { attackType = Normal, blockType = Ghost, effectiveness = Ineffective }
    , { attackType = Normal, blockType = Steel, effectiveness = NotVeryEffective }
      -- Fight
    , { attackType = Fight, blockType = Normal, effectiveness = SuperEffective }
    , { attackType = Fight, blockType = Flying, effectiveness = NotVeryEffective }
    , { attackType = Fight, blockType = Poison, effectiveness = NotVeryEffective }
    , { attackType = Fight, blockType = Rock, effectiveness = SuperEffective }
    , { attackType = Fight, blockType = Bug, effectiveness = NotVeryEffective }
    , { attackType = Fight, blockType = Ghost, effectiveness = Ineffective }
    , { attackType = Fight, blockType = Steel, effectiveness = SuperEffective }
    , { attackType = Fight, blockType = Psychc, effectiveness = NotVeryEffective }
    , { attackType = Fight, blockType = Ice, effectiveness = SuperEffective }
    , { attackType = Fight, blockType = Dark, effectiveness = SuperEffective }
    , { attackType = Fight, blockType = Fairy, effectiveness = NotVeryEffective }
      -- Flying
    , { attackType = Flying, blockType = Fight, effectiveness = SuperEffective }
    , { attackType = Flying, blockType = Rock, effectiveness = NotVeryEffective }
    , { attackType = Flying, blockType = Bug, effectiveness = SuperEffective }
    , { attackType = Flying, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Flying, blockType = Grass, effectiveness = SuperEffective }
    , { attackType = Flying, blockType = Electr, effectiveness = NotVeryEffective }
      -- Poison
    , { attackType = Poison, blockType = Poison, effectiveness = NotVeryEffective }
    , { attackType = Poison, blockType = Ground, effectiveness = NotVeryEffective }
    , { attackType = Poison, blockType = Rock, effectiveness = NotVeryEffective }
    , { attackType = Poison, blockType = Ghost, effectiveness = NotVeryEffective }
    , { attackType = Poison, blockType = Steel, effectiveness = Ineffective }
    , { attackType = Poison, blockType = Grass, effectiveness = SuperEffective }
    , { attackType = Poison, blockType = Fairy, effectiveness = SuperEffective }
      -- Ground
    , { attackType = Ground, blockType = Flying, effectiveness = Ineffective }
    , { attackType = Ground, blockType = Poison, effectiveness = SuperEffective }
    , { attackType = Ground, blockType = Rock, effectiveness = SuperEffective }
    , { attackType = Ground, blockType = Bug, effectiveness = NotVeryEffective }
    , { attackType = Ground, blockType = Steel, effectiveness = SuperEffective }
    , { attackType = Ground, blockType = Fire, effectiveness = SuperEffective }
    , { attackType = Ground, blockType = Grass, effectiveness = NotVeryEffective }
    , { attackType = Ground, blockType = Electr, effectiveness = SuperEffective }
      -- Rock
    , { attackType = Rock, blockType = Fight, effectiveness = NotVeryEffective }
    , { attackType = Rock, blockType = Flying, effectiveness = SuperEffective }
    , { attackType = Rock, blockType = Ground, effectiveness = NotVeryEffective }
    , { attackType = Rock, blockType = Bug, effectiveness = SuperEffective }
    , { attackType = Rock, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Rock, blockType = Fire, effectiveness = SuperEffective }
    , { attackType = Rock, blockType = Ice, effectiveness = SuperEffective }
      -- Bug
    , { attackType = Bug, blockType = Fight, effectiveness = NotVeryEffective }
    , { attackType = Bug, blockType = Flying, effectiveness = NotVeryEffective }
    , { attackType = Bug, blockType = Poison, effectiveness = NotVeryEffective }
    , { attackType = Bug, blockType = Ghost, effectiveness = NotVeryEffective }
    , { attackType = Bug, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Bug, blockType = Fire, effectiveness = NotVeryEffective }
    , { attackType = Bug, blockType = Grass, effectiveness = SuperEffective }
    , { attackType = Bug, blockType = Psychc, effectiveness = SuperEffective }
    , { attackType = Bug, blockType = Dark, effectiveness = SuperEffective }
    , { attackType = Bug, blockType = Fairy, effectiveness = NotVeryEffective }
      -- Ghost
    , { attackType = Ghost, blockType = Normal, effectiveness = Ineffective }
    , { attackType = Ghost, blockType = Ghost, effectiveness = SuperEffective }
    , { attackType = Ghost, blockType = Psychc, effectiveness = SuperEffective }
    , { attackType = Ghost, blockType = Dark, effectiveness = NotVeryEffective }
      -- Steel
    , { attackType = Steel, blockType = Rock, effectiveness = SuperEffective }
    , { attackType = Steel, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Steel, blockType = Fire, effectiveness = NotVeryEffective }
    , { attackType = Steel, blockType = Water, effectiveness = NotVeryEffective }
    , { attackType = Steel, blockType = Electr, effectiveness = NotVeryEffective }
    , { attackType = Steel, blockType = Ice, effectiveness = SuperEffective }
    , { attackType = Steel, blockType = Fairy, effectiveness = SuperEffective }
      -- Fire
    , { attackType = Fire, blockType = Rock, effectiveness = NotVeryEffective }
    , { attackType = Fire, blockType = Bug, effectiveness = SuperEffective }
    , { attackType = Fire, blockType = Steel, effectiveness = SuperEffective }
    , { attackType = Fire, blockType = Fire, effectiveness = NotVeryEffective }
    , { attackType = Fire, blockType = Water, effectiveness = NotVeryEffective }
    , { attackType = Fire, blockType = Grass, effectiveness = SuperEffective }
    , { attackType = Fire, blockType = Ice, effectiveness = SuperEffective }
    , { attackType = Fire, blockType = Dragon, effectiveness = NotVeryEffective }
      -- Water
    , { attackType = Water, blockType = Ground, effectiveness = SuperEffective }
    , { attackType = Water, blockType = Rock, effectiveness = SuperEffective }
    , { attackType = Water, blockType = Fire, effectiveness = SuperEffective }
    , { attackType = Water, blockType = Water, effectiveness = NotVeryEffective }
    , { attackType = Water, blockType = Grass, effectiveness = NotVeryEffective }
    , { attackType = Water, blockType = Dragon, effectiveness = NotVeryEffective }
      -- Grass
    , { attackType = Grass, blockType = Flying, effectiveness = NotVeryEffective }
    , { attackType = Grass, blockType = Poison, effectiveness = NotVeryEffective }
    , { attackType = Grass, blockType = Ground, effectiveness = SuperEffective }
    , { attackType = Grass, blockType = Rock, effectiveness = SuperEffective }
    , { attackType = Grass, blockType = Bug, effectiveness = NotVeryEffective }
    , { attackType = Grass, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Grass, blockType = Fire, effectiveness = NotVeryEffective }
    , { attackType = Grass, blockType = Water, effectiveness = SuperEffective }
    , { attackType = Grass, blockType = Grass, effectiveness = NotVeryEffective }
    , { attackType = Grass, blockType = Dragon, effectiveness = NotVeryEffective }
      -- Electr
    , { attackType = Electr, blockType = Flying, effectiveness = SuperEffective }
    , { attackType = Electr, blockType = Ground, effectiveness = Ineffective }
    , { attackType = Electr, blockType = Water, effectiveness = SuperEffective }
    , { attackType = Electr, blockType = Grass, effectiveness = NotVeryEffective }
    , { attackType = Electr, blockType = Electr, effectiveness = NotVeryEffective }
    , { attackType = Electr, blockType = Dragon, effectiveness = NotVeryEffective }
      -- Psychc
    , { attackType = Psychc, blockType = Fight, effectiveness = SuperEffective }
    , { attackType = Psychc, blockType = Poison, effectiveness = SuperEffective }
    , { attackType = Psychc, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Psychc, blockType = Psychc, effectiveness = NotVeryEffective }
    , { attackType = Psychc, blockType = Dark, effectiveness = Ineffective }
      -- Ice
    , { attackType = Ice, blockType = Flying, effectiveness = SuperEffective }
    , { attackType = Ice, blockType = Ground, effectiveness = SuperEffective }
    , { attackType = Ice, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Ice, blockType = Fire, effectiveness = NotVeryEffective }
    , { attackType = Ice, blockType = Water, effectiveness = NotVeryEffective }
    , { attackType = Ice, blockType = Grass, effectiveness = SuperEffective }
    , { attackType = Ice, blockType = Ice, effectiveness = NotVeryEffective }
    , { attackType = Ice, blockType = Dragon, effectiveness = SuperEffective }
      -- Dragon
    , { attackType = Dragon, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Dragon, blockType = Dragon, effectiveness = SuperEffective }
    , { attackType = Dragon, blockType = Fairy, effectiveness = Ineffective }
      -- Dark
    , { attackType = Dark, blockType = Fight, effectiveness = NotVeryEffective }
    , { attackType = Dark, blockType = Ghost, effectiveness = SuperEffective }
    , { attackType = Dark, blockType = Psychc, effectiveness = SuperEffective }
    , { attackType = Dark, blockType = Dark, effectiveness = NotVeryEffective }
    , { attackType = Dark, blockType = Fairy, effectiveness = NotVeryEffective }
      -- Fairy
    , { attackType = Fairy, blockType = Fight, effectiveness = SuperEffective }
    , { attackType = Fairy, blockType = Poison, effectiveness = NotVeryEffective }
    , { attackType = Fairy, blockType = Steel, effectiveness = NotVeryEffective }
    , { attackType = Fairy, blockType = Fire, effectiveness = NotVeryEffective }
    , { attackType = Fairy, blockType = Dragon, effectiveness = SuperEffective }
    , { attackType = Fairy, blockType = Dark, effectiveness = SuperEffective }
    ]


lookupEffective : PokeType -> PokeType -> Effectiveness
lookupEffective attackType blockType =
    case List.head (List.filter (\c -> attackType == c.attackType && blockType == c.blockType) typeChart) of
        Nothing ->
            Effective

        Just c ->
            c.effectiveness


type alias ModelRow =
    { attackType : PokeType, blockType : PokeType, effectiveness : Effectiveness }


type alias Model =
    List ModelRow


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


product : List a -> List b -> List ( a, b )
product xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


initModel : Model
initModel =
    List.map (\t -> { attackType = (Tuple.first t), blockType = (Tuple.second t), effectiveness = (lookupEffective (Tuple.first t) (Tuple.second t)) }) (product pokeTypes pokeTypes)



-- MESSAGES


type Msg
    = NoOp



-- VIEW


textDiv txt =
    Html.div [] [ text txt ]


rowStyle =
    [ style [ ( "border", "1px #707070 solid" ) ] ]


viewHeaderRows : Html Msg
viewHeaderRows =
    tr rowStyle
        [ (th rowStyle [ text "こうげき" ])
        , (th rowStyle [ text "ぼうぎょ" ])
        , (th rowStyle [ text "こうか" ])
        ]


viewRows : ModelRow -> Html Msg
viewRows row =
    tr rowStyle
        [ (td rowStyle [ text (pokeTypeJa row.attackType) ])
        , (td rowStyle [ text (pokeTypeJa row.blockType) ])
        , (td rowStyle [ text (effectivenessStr row.effectiveness) ])
        ]


view : Model -> Html Msg
view model =
    div []
        [ table [ style [ ( "border", "2px #303030 solid" ) ] ]
            (List.append
                [ viewHeaderRows ]
                (List.map viewRows model)
            )
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
