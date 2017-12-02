module Main exposing (..)

import Element exposing (text)
import Html exposing (Html)
import Http
import HttpBuilder
import Json.Decode
import Json.Decode.Pipeline
import RemoteData exposing (WebData)
import Style exposing (..)
import Style.Font as Font
import Task


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



---- REQUESTS ----


decodeChampion : Json.Decode.Decoder Champion
decodeChampion =
    Json.Decode.Pipeline.decode Champion
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.custom (Json.Decode.at [ "image", "full" ] Json.Decode.string)
        |> Json.Decode.Pipeline.required "id" Json.Decode.string


decodeChampions : Json.Decode.Decoder (List Champion)
decodeChampions =
    Json.Decode.map (List.map Tuple.second) <|
        Json.Decode.at [ "data" ] <|
            Json.Decode.keyValuePairs decodeChampion


getChampions : Task.Task Never (WebData (List Champion))
getChampions =
    HttpBuilder.get "http://ddragon.leagueoflegends.com/cdn/6.24.1/data/en_US/champion.json"
        |> HttpBuilder.withExpect (Http.expectJson decodeChampions)
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
    }


type StyleClass
    = NoStyle
    | Header


sheet : StyleSheet StyleClass variation
sheet =
    Style.styleSheet
        [ style NoStyle []
        , style Header
            [ Font.size 24
            , Font.uppercase
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
                text (toString error)

            RemoteData.Success champs ->
                Element.paragraph NoStyle
                    []
                    [ text (toString champs)
                    ]

            RemoteData.Loading ->
                text "Loading......PLEASE WAIT"

            _ ->
                text ""



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
