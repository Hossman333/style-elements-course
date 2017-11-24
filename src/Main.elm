module Main exposing (..)

import Color exposing (black, rgba, white)
import Element exposing (Device, classifyDevice, column, el, empty, h1, h2, image, modal, node, paragraph, row, screen, text, textLayout, within, wrappedColumn, wrappedRow)
import Element.Attributes exposing (alignLeft, alignRight, center, fill, height, inlineStyle, paddingBottom, paddingTop, paddingXY, percent, px, spacing, spread, verticalCenter, width)
import Element.Events exposing (onClick)
import Html exposing (Html)
import Http exposing (Part, decodeUri, encodeUri, expectJson)
import HttpBuilder exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import Navigation
import RemoteData exposing (WebData)
import Style exposing (..)
import Style.Color
import Style.Font as Font exposing (lineHeight)
import Style.Transition exposing (transitions)
import Task exposing (..)
import Time
import UrlParser exposing (..)
import Window


---- MODEL ----


type alias Model =
    { route : Route
    , location : Navigation.Location
    , champions : WebData (List Champion)
    , device : Device
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        route =
            fromLocation location

        device =
            classifyDevice flags.windowSize

        model =
            { route = route
            , location = location
            , champions = RemoteData.Loading
            , device = device
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



---- ROUTER ----


type Route
    = Home
    | Modal String


urlParser : Parser (Route -> c) c
urlParser =
    UrlParser.oneOf
        [ UrlParser.map Home (s "")
        , UrlParser.map Modal UrlParser.string
        ]


toUrl : Route -> String
toUrl route =
    case route of
        Home ->
            "/"

        Modal champName ->
            "/" ++ champName


goTo : Route -> Cmd a
goTo route =
    Navigation.newUrl <| toUrl route


fromLocation : Navigation.Location -> Route
fromLocation location =
    Maybe.withDefault Home <|
        parsePath urlParser location



---- UPDATE ----


type Msg
    = NoOp
    | LocationChange Navigation.Location
    | GotChampions (WebData (List Champion))
    | GoTo Route
    | ResizeWindow Window.Size


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

        GoTo newLoc ->
            model ! [ goTo newLoc ]

        LocationChange loc ->
            let
                newRoute =
                    fromLocation loc
            in
            { model | route = newRoute, location = loc } ! []

        ResizeWindow windowSize ->
            { model | device = classifyDevice windowSize } ! []

        NoOp ->
            model ! []



---- VIEW ----


renderChampions champs =
    champs
        |> List.map
            (\champ ->
                column ChampionStyles
                    [ inlineStyle
                        [ ( "position", "relative" )
                        ]
                    , onClick <| GoTo (Modal (encodeUri (String.toLower champ.name)))
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


view : Model -> Html Msg
view model =
    Element.layout sheet <|
        case model.route of
            Home ->
                case model.champions of
                    RemoteData.Failure error ->
                        text (toString error)

                    RemoteData.Loading ->
                        text "Loading..."

                    RemoteData.Success champs ->
                        renderChampions champs

                    _ ->
                        text ""

            Modal champName ->
                let
                    removeExtension champ =
                        List.head (String.split "." champ.image)

                    overlay =
                        screen <|
                            el Overlay
                                [ width (percent 100)
                                , height (percent 100)
                                , onClick <| GoTo Home
                                ]
                                empty
                in
                case model.champions of
                    RemoteData.Failure error ->
                        text (toString error)

                    RemoteData.Loading ->
                        text "Loading..."

                    RemoteData.Success champs ->
                        let
                            decodedChampNameM =
                                decodeUri champName

                            champM =
                                List.head
                                    (List.filter
                                        (\champ ->
                                            case decodedChampNameM of
                                                Just decodedChampName ->
                                                    decodedChampName == String.toLower champ.name

                                                Nothing ->
                                                    False
                                        )
                                        champs
                                    )
                        in
                        case champM of
                            Just champ ->
                                column NoStyle
                                    []
                                    [ if model.device.phone then
                                        empty
                                      else
                                        renderChampions champs
                                    , if model.device.phone then
                                        column NoStyle
                                            []
                                            [ image NoStyle
                                                [ center ]
                                                { src =
                                                    case removeExtension champ of
                                                        Just champImg ->
                                                            "http://ddragon.leagueoflegends.com/cdn/img/champion/loading/" ++ champImg ++ "_0.jpg"

                                                        Nothing ->
                                                            ""
                                                , caption = champ.name
                                                }
                                            , h1 NoStyle [ inlineStyle [ ( "font-size", "48px" ) ] ] <| text champ.name
                                            , h2 NoStyle [ inlineStyle [ ( "font-size", "22px" ) ] ] <| text champ.title
                                            , paragraph NoStyle
                                                [ inlineStyle
                                                    [ ( "font-size", "13px" )
                                                    , ( "text-align", "left" )
                                                    , ( "color", "#5D6467" )
                                                    ]
                                                , paddingXY 25 15
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
                                      else
                                        within
                                            [ overlay ]
                                        <|
                                            modal
                                                NoStyle
                                                [ center
                                                , verticalCenter
                                                , width (percent 50)
                                                ]
                                            <|
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
                                                            , paddingXY 0 10
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
                                    ]

                            Nothing ->
                                text "Sorry, no champion!"

                    _ ->
                        text ""



---- PROGRAM ----


type alias Flags =
    { windowSize : Window.Size
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\size -> ResizeWindow size)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
