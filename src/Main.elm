module Main exposing (..)

import Color exposing (black, rgba, white)
import Element exposing (column, el, empty, h1, h2, image, modal, node, paragraph, row, screen, text, textLayout, within, wrappedColumn, wrappedRow)
import Element.Attributes exposing (alignLeft, alignRight, center, fill, height, inlineStyle, paddingBottom, paddingTop, paddingXY, percent, px, spacing, spread, verticalCenter, width)
import Element.Events exposing (onClick)
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
    , isModalOpen : Bool
    , champSelected : Maybe Champion
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { champions = RemoteData.Loading
            , isModalOpen = False
            , champSelected = Nothing
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
    | ToggleModal (Maybe Champion)


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
    | MainFont
    | ChampionStyles
    | ChampionName
    | Overlay


sheet : StyleSheet StyleClass variation
sheet =
    Style.styleSheet
        [ style NoStyle []
        , style MainFont
            [ Font.size 14
            ]
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
        , style Overlay
            [ Style.Color.background (rgba 0 0 0 0.7)
            , cursor "pointer"
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotChampions webChampionData ->
            { model | champions = webChampionData } ! []

        ToggleModal champM ->
            { model | isModalOpen = not model.isModalOpen, champSelected = champM } ! []

        NoOp ->
            model ! []



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        removeExtension champ =
            List.head (String.split "." champ.image)

        overlay =
            screen <|
                el Overlay
                    [ width (percent 100)
                    , height (percent 100)
                    , onClick <| ToggleModal Nothing
                    ]
                    empty
    in
    Element.layout sheet <|
        column NoStyle
            []
            [ if model.isModalOpen then
                within
                    [ overlay ]
                <|
                    modal NoStyle
                        [ center
                        , verticalCenter
                        , width (percent 50)
                        ]
                    <|
                        case model.champSelected of
                            Just champ ->
                                column NoStyle
                                    [ inlineStyle
                                        [ ( "background-color", "#fff" )
                                        , ( "border-radius", "6px" )
                                        ]
                                    ]
                                    [ row NoStyle
                                        [ spread
                                        , spacing 20
                                        ]
                                        [ column NoStyle
                                            [ alignLeft
                                            ]
                                            [ image NoStyle
                                                []
                                                { src =
                                                    case removeExtension champ of
                                                        Just champImg ->
                                                            "http://ddragon.leagueoflegends.com/cdn/img/champion/loading/" ++ champImg ++ "_0.jpg"

                                                        Nothing ->
                                                            ""
                                                , caption = champ.name
                                                }
                                            ]
                                        , wrappedColumn NoStyle
                                            [ alignLeft
                                            , paddingXY 0 0
                                            ]
                                            [ h1 NoStyle [ inlineStyle [ ( "font-size", "48px" ) ] ] <| text champ.name
                                            , h2 NoStyle [ inlineStyle [ ( "font-size", "22px" ) ] ] <| text champ.title
                                            , paragraph NoStyle
                                                [ inlineStyle
                                                    [ ( "font-size", "13px" )
                                                    , ( "text-align", "left" )
                                                    , ( "color", "#5D6467" )
                                                    ]
                                                , paddingXY 0 15
                                                ]
                                                [ text champ.blurb
                                                ]
                                            , el MainFont [ paddingBottom 2 ] <| text ("Attack: " ++ toString champ.info.attack)
                                            , el NoStyle
                                                [ inlineStyle
                                                    [ ( "background-color", "#50E3C2" )
                                                    , ( "height", "6px" )
                                                    , ( "width", toString champ.info.attack ++ "0%" )
                                                    ]
                                                ]
                                              <|
                                                empty
                                            , el MainFont [ paddingBottom 2, paddingTop 8 ] <| text ("Defense: " ++ toString champ.info.defense)
                                            , el NoStyle
                                                [ inlineStyle
                                                    [ ( "background-color", "#F5CD1A" )
                                                    , ( "height", "6px" )
                                                    , ( "width", toString champ.info.defense ++ "0%" )
                                                    ]
                                                ]
                                              <|
                                                empty
                                            , el MainFont [ paddingBottom 2, paddingTop 8 ] <| text ("Magic: " ++ toString champ.info.magic)
                                            , el NoStyle
                                                [ inlineStyle
                                                    [ ( "background-color", "#44C0FF" )
                                                    , ( "height", "6px" )
                                                    , ( "width", toString champ.info.magic ++ "0%" )
                                                    ]
                                                ]
                                              <|
                                                empty
                                            , el MainFont [ paddingBottom 2, paddingTop 8 ] <| text ("Difficulty: " ++ toString champ.info.difficulty)
                                            , el NoStyle
                                                [ inlineStyle
                                                    [ ( "background-color", "#EA4F62" )
                                                    , ( "height", "6px" )
                                                    , ( "width", toString champ.info.difficulty ++ "0%" )
                                                    ]
                                                ]
                                              <|
                                                empty
                                            , row MainFont [ paddingTop 20, inlineStyle [ ( "color", "#5D6467" ) ] ] <|
                                                (el NoStyle
                                                    []
                                                 <|
                                                    text "Type: "
                                                )
                                                    :: List.indexedMap
                                                        (\index champTag ->
                                                            if index == (List.length champ.tags - 1) then
                                                                el NoStyle [ inlineStyle [ ( "font-weight", "bold" ) ] ] <| text champTag
                                                            else
                                                                el NoStyle [ inlineStyle [ ( "font-weight", "bold" ) ] ] <| text (champTag ++ ", ")
                                                        )
                                                        champ.tags
                                            ]
                                        ]
                                    ]

                            Nothing ->
                                text "Error selecting champion..."
              else
                empty
            , case model.champions of
                RemoteData.Failure error ->
                    text (toString error)

                RemoteData.Loading ->
                    text "Loading..."

                RemoteData.Success champs ->
                    champs
                        |> List.map
                            (\champ ->
                                column ChampionStyles
                                    [ inlineStyle
                                        [ ( "position", "relative" )
                                        ]
                                    , onClick <| ToggleModal (Just champ)
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
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
