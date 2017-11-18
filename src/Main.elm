module Main exposing (..)

import Color exposing (black, white)
import Element exposing (column, el, image, node, paragraph, text, wrappedRow)
import Element.Attributes exposing (center, height, inlineStyle, px, verticalCenter, width)
import Html exposing (Html)
import Http exposing (expectJson)
import HttpBuilder exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import RemoteData exposing (WebData)
import Style exposing (..)
import Style.Color
import Style.Font as Font exposing (lineHeight)
import Style.Transition exposing (transitions)
import Task exposing (..)
import Time


---- MODEL ----


type alias Model =
    { champions : WebData (List Champion)
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { champions = RemoteData.Loading
            }
    in
    model
        ! [ getChampions
                |> Task.perform GotChampions
          ]


decodeChampionInfo : Json.Decode.Decoder ChampionInfo
decodeChampionInfo =
    Json.Decode.Pipeline.decode ChampionInfo
        |> Json.Decode.Pipeline.required "attack" Json.Decode.int
        |> Json.Decode.Pipeline.required "defense" Json.Decode.int
        |> Json.Decode.Pipeline.required "magic" Json.Decode.int
        |> Json.Decode.Pipeline.required "difficulty" Json.Decode.int


decodeChampion : Json.Decode.Decoder Champion
decodeChampion =
    Json.Decode.Pipeline.decode Champion
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.custom
            (Json.Decode.at [ "image", "full" ] Json.Decode.string)
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "blurb" Json.Decode.string
        |> Json.Decode.Pipeline.required "info" decodeChampionInfo
        |> Json.Decode.Pipeline.required "tags" (Json.Decode.list Json.Decode.string)


decodeChampions : Json.Decode.Decoder (List Champion)
decodeChampions =
    Json.Decode.map (List.map Tuple.second) <|
        Json.Decode.at [ "data" ] <|
            Json.Decode.keyValuePairs decodeChampion


getChampions : Task Never (WebData (List Champion))
getChampions =
    HttpBuilder.get "http://ddragon.leagueoflegends.com/cdn/6.24.1/data/en_US/champion.json"
        |> HttpBuilder.withExpect (Http.expectJson <| decodeChampions)
        |> HttpBuilder.toTask
        |> RemoteData.fromTask



---- UPDATE ----


type Msg
    = NoOp
    | GotChampions (WebData (List Champion))


type alias Champion =
    { name : String
    , title : String
    , image : String
    , id : String
    , blurb : String
    , info : ChampionInfo
    , tags : List String
    }


type alias ChampionInfo =
    { attack : Int
    , defense : Int
    , magic : Int
    , difficulty : Int
    }


type StyleClass
    = NoStyle
    | ChampionStyles
    | ChampionName


sheet : StyleSheet StyleClass variation
sheet =
    Style.styleSheet
        [ style NoStyle []
        , style ChampionStyles []
        , style ChampionName
            [ Font.size 14
            , Style.Color.text white
            , opacity 0
            , transitions
                [ { delay = 0
                  , duration = 300 * Time.millisecond
                  , easing = "ease"
                  , props = [ "opacity" ]
                  }
                ]
            , hover
                [ opacity 1
                , cursor "pointer"
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotChampions webChampionData ->
            { model | champions = webChampionData } ! []

        NoOp ->
            model ! []



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout sheet <|
        case model.champions of
            RemoteData.Failure error ->
                Debug.crash (toString error)

            RemoteData.Success champs ->
                champs
                    |> List.map
                        (\champ ->
                            column ChampionStyles
                                [ inlineStyle
                                    [ ( "position", "relative" )
                                    ]
                                ]
                                [ image
                                    NoStyle
                                    []
                                    { src = "http://ddragon.leagueoflegends.com/cdn/6.24.1/img/champion/" ++ champ.image
                                    , caption = champ.name
                                    }
                                , paragraph ChampionName
                                    [ width (px 120)
                                    , height (px 120)
                                    , inlineStyle
                                        [ ( "position", "absolute" )
                                        , ( "top", "0" )
                                        , ( "line-height", "120px" )
                                        , ( "background-color", "rgba(0,0,0,0.8)" )
                                        ]
                                    ]
                                    [ text champ.name ]
                                ]
                        )
                    |> wrappedRow NoStyle [ center ]

            _ ->
                text ""



-- Element.h1 Header [] <|
--     text "There's a snake in my boot"
---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
