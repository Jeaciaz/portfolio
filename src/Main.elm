port module Main exposing (..)

import Browser
import Browser.Events as BrowserEvent
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Simple.Transition as Transition
import Theme
import Time
import Util



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- PORTS


port scrollToID : String -> Cmd msg


port toggleScrollLock : Bool -> Cmd msg


port setVisited : () -> Cmd msg



-- MODEL


typewrittenTexts : List (List TypewriterBlock)
typewrittenTexts =
    [ [ Text "Hello there!" ]
    , [ Text "My name is Sergei Knyazev. I am a passionate frontend developer who loves delivering top-quality web applications and takes pride in his work." ]
    , [ Text "I primarily work in React and Typescript, but outside of my full-time job I love using functional languages with sound type systems, such as Elm or Haskell. In fact, this very website has been "
      , Link "https://github.com/Jeaciaz/portfolio" "written in Elm!"
      ]
    ]


cards : List CardInfo
cards =
    [ { title = "Restaurant list"
      , href = "https://jeaciaz.github.io/restaurant-list/"
      , githubHref = "https://github.com/Jeaciaz/restaurant-list"
      , desc = "A simplistic table with a list of Moscow restaurants recommended by a colleague of mine."
      , techStack =
            [ "HTML + CSS (Bootstrap)"
            , "Vanilla JS"
            ]
      }
    , { title = "Auth mocker"
      , href = "https://jeaciaz.github.io/auth-mocker/"
      , githubHref = "https://github.com/Jeaciaz/auth-mocker"
      , desc = "Implementation of an auth form design I found exciting."
      , techStack =
            [ "React"
            , "PostCSS Modules"
            , "Vite"
            ]
      }
    ]


type TypewriterStatus
    = Incomplete (List TypewriterFragment)
    | AwaitingKeypress (List (List TypewriterBlock))
    | Complete (List (List TypewriterBlock))


type alias Model =
    { theme : Theme.Theme
    , viewportHeight : Int
    , typewriterStatus : TypewriterStatus
    , typewriterSpeed : TypewriterSpeed
    , deviceClass : DeviceClass
    }


type alias Flags =
    { innerHeight : Int, innerWidth : Int, isVisited : Bool }


type alias Href =
    String


type TypewriterBlock
    = Link Href String
    | Text String


type alias TypewriterFragment =
    ( Int, List TypewriterBlock )


type TypewriterSpeed
    = Normal
    | Fast
    | Fastest


type alias CardInfo =
    { title : String
    , href : String
    , githubHref : String
    , desc : String
    , techStack : List String
    }


typewriterBlockLength : TypewriterBlock -> Int
typewriterBlockLength frag =
    case frag of
        Link _ str ->
            String.length str

        Text str ->
            String.length str


typewriterBlockListLength : List TypewriterBlock -> Int
typewriterBlockListLength =
    List.foldl (typewriterBlockLength >> (+)) 0


cutTypewriterFragment : TypewriterFragment -> List TypewriterBlock
cutTypewriterFragment ( limit, frags ) =
    Util.mapAcc
        (\frag acc ->
            case frag of
                Text t ->
                    ( Text <| String.left acc t, max 0 (acc - String.length t) )

                Link href t ->
                    ( Link href <| String.left acc t, max 0 (acc - String.length t) )
        )
        limit
        frags
        |> Tuple.first


unwrapFragments : List TypewriterFragment -> List (List TypewriterBlock)
unwrapFragments =
    List.map Tuple.second


init : Flags -> ( Model, Cmd msg )
init { innerHeight, innerWidth, isVisited } =
    if isVisited then
        ( Model Theme.light innerHeight (Complete typewrittenTexts) Normal (.class <| classifyDevice { width = innerWidth, height = innerHeight })
        , Cmd.none
        )

    else
        ( Model Theme.light innerHeight (Incomplete (List.map (\p -> ( 0, p )) typewrittenTexts)) Normal (.class <| classifyDevice { width = innerWidth, height = innerHeight })
        , toggleScrollLock True
        )



-- UPDATE


type Msg
    = TypewriterTick
    | SpeedUp
    | UpdateViewport Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypewriterTick ->
            case model.typewriterStatus of
                Incomplete fragments ->
                    let
                        -- separate completed paragraphs and still in progress
                        ( completed, uncompleted ) =
                            List.partition (\( len, p ) -> len >= typewriterBlockListLength p) fragments

                        -- increment the first found incomplete paragraph char count
                        updatedUncompleted =
                            case uncompleted of
                                [] ->
                                    []

                                ( len, str ) :: tail ->
                                    ( len + 1, str ) :: tail

                        isDone =
                            case updatedUncompleted of
                                [] ->
                                    True

                                [ ( len, p ) ] ->
                                    len == typewriterBlockListLength p

                                _ ->
                                    False
                    in
                    ( { model
                        | typewriterStatus =
                            if isDone then
                                AwaitingKeypress <| unwrapFragments (completed ++ updatedUncompleted)

                            else
                                Incomplete <| completed ++ updatedUncompleted
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SpeedUp ->
            let
                newModel =
                    case ( model.typewriterStatus, model.typewriterSpeed ) of
                        ( AwaitingKeypress texts, _ ) ->
                            { model | typewriterStatus = Complete texts }

                        ( _, Normal ) ->
                            { model | typewriterSpeed = Fast }

                        ( _, Fast ) ->
                            { model | typewriterSpeed = Fastest }

                        ( _, Fastest ) ->
                            { model
                                | typewriterStatus =
                                    case model.typewriterStatus of
                                        Incomplete fragmentList ->
                                            AwaitingKeypress <| unwrapFragments fragmentList

                                        AwaitingKeypress blocks ->
                                            Complete blocks

                                        Complete blocks ->
                                            Complete blocks
                            }
            in
            ( newModel
            , case newModel.typewriterStatus of
                Complete _ ->
                    Cmd.batch
                        [ scrollToID recentWorkId
                        , setVisited ()
                        , toggleScrollLock False
                        ]

                _ ->
                    Cmd.none
            )

        UpdateViewport newWidth newHeight ->
            ( { model | viewportHeight = newHeight, deviceClass = .class <| classifyDevice { width = newWidth, height = newHeight } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        speedInMs =
            case model.typewriterSpeed of
                Fastest ->
                    8

                Fast ->
                    25

                Normal ->
                    85
    in
    Sub.batch
        [ case model.typewriterStatus of
            Incomplete _ ->
                Sub.batch
                    [ Time.every speedInMs (\_ -> TypewriterTick)
                    , BrowserEvent.onKeyPress (Decode.succeed SpeedUp)
                    ]

            AwaitingKeypress _ ->
                BrowserEvent.onKeyPress (Decode.succeed SpeedUp)

            Complete _ ->
                Sub.none
        , BrowserEvent.onResize UpdateViewport
        ]



-- VIEW


montserrat : Attribute msg
montserrat =
    Font.family [ Font.typeface "Montserrat", Font.sansSerif ]


roboto : Attribute msg
roboto =
    Font.family [ Font.typeface "Roboto", Font.monospace ]


recentWorkId : String
recentWorkId =
    "recent-work"



-- | example use: el [ transition [ Transition.color, Transition.backgroundColor ] ]


transition : List (Transition.Millis -> List Transition.Option -> Transition.Property) -> Attribute msg
transition =
    Transition.all { duration = 100, options = [ Transition.easeInOut ] } >> htmlAttribute


view : Model -> Html Msg
view model =
    layout
        [ montserrat
        , Font.size (Theme.fontSizeText model.deviceClass)
        , Background.color
            model.theme.background
        , Font.color model.theme.text
        ]
    <|
        column
            ([ width fill
             , height fill
             ]
                ++ (case model.typewriterStatus of
                        Complete _ ->
                            []

                        _ ->
                            [ clip ]
                   )
            )
            [ viewTerminal model
            , column
                [ htmlAttribute (Attr.id recentWorkId)
                , height
                    (if model.deviceClass == Phone then
                        fill

                     else
                        fill |> minimum model.viewportHeight
                    )
                , width fill
                ]
                [ column
                    [ alignTop
                    , centerX
                    , paddingEach { top = 80, right = 24, bottom = 12, left = 24 }
                    , spacing 12
                    ]
                    [ el
                        [ Font.color model.theme.primary
                        , centerX
                        ]
                        (viewH2 model (text "My recent work"))
                    , paragraph [] [ text "My GitHub doesn’t have much activity due to my work being mostly on my day job. These are things I made in my free time that I don’t consider obsolete." ]
                    ]
                , wrappedRow [ clip, centerX, centerY, spacing 24 ] <| List.map (viewCard model) cards
                , viewFooter model
                ]
            ]


viewTerminal : Model -> Element Msg
viewTerminal model =
    column
        ([ width fill
         , height <| px model.viewportHeight
         , centerX
         , padding 24
         , Font.color model.theme.background
         , Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
         , Font.bold
         , Background.color model.theme.primary
         ]
            ++ (case model.typewriterStatus of
                    Complete _ ->
                        []

                    _ ->
                        [ Events.onClick SpeedUp ]
               )
        )
        [ column [ centerX, centerY, width (fill |> maximum 640), height (fill |> maximum 480), spacing 24 ] <|
            let
                visibleBlocks : List ( Bool, List TypewriterBlock )
                visibleBlocks =
                    case model.typewriterStatus of
                        Incomplete frags ->
                            List.filterMap
                                (\frag ->
                                    if Tuple.first frag /= 0 then
                                        Just ( Tuple.first frag < (typewriterBlockListLength <| Tuple.second frag), cutTypewriterFragment frag )

                                    else
                                        Nothing
                                )
                                frags

                        AwaitingKeypress blocks ->
                            List.map (\block -> ( False, block )) blocks

                        Complete blocks ->
                            List.map (\block -> ( False, block )) blocks
            in
            List.map
                (\( shouldViewCaret, block ) ->
                    paragraph []
                        ((block
                            |> List.map (viewTypewriterFragment model)
                         )
                            ++ [ if shouldViewCaret then
                                    viewCaret model

                                 else
                                    none
                               ]
                        )
                )
                visibleBlocks
                ++ [ paragraph [ paddingXY 0 24 ]
                        (case model.typewriterStatus of
                            Incomplete _ ->
                                [ text
                                    ("Press any key to "
                                        ++ (case model.typewriterSpeed of
                                                Normal ->
                                                    "speed up"

                                                Fast ->
                                                    "speed up again"

                                                Fastest ->
                                                    "skip"
                                           )
                                    )
                                , viewCaret model
                                ]

                            AwaitingKeypress _ ->
                                [ text "Press any key to continue", viewCaret model ]

                            Complete _ ->
                                []
                        )
                   ]
        ]


viewTypewriterFragment : Model -> TypewriterBlock -> Element Msg
viewTypewriterFragment { theme } frag =
    case frag of
        Text t ->
            text t

        Link href t ->
            newTabLink [] { url = href, label = viewLink theme.hyperlink theme.text t }


viewCard : Model -> CardInfo -> Element Msg
viewCard { theme, deviceClass } { title, href, githubHref, desc, techStack } =
    el [ padding 12, width (fillPortion 1 |> maximum 400), height fill ] <|
        column
            [ height fill
            , Background.color theme.white
            , Border.rounded 8
            , Border.shadow { offset = ( 2.0, 2.0 ), size = 1.0, blur = 4.0, color = theme.text |> Theme.addOpacity 0.5 }
            ]
            [ paragraph
                [ Font.center
                , spacing 8
                , paddingEach { top = 24, right = 24, bottom = 12, left = 24 }
                , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
                , Border.color theme.background
                ]
                [ newTabLink
                    [ Font.size <| Theme.fontSizeH3 deviceClass
                    , Font.semiBold
                    , montserrat
                    ]
                    { url = href, label = viewLink theme.hyperlink theme.secondary title }
                , text " "
                , newTabLink [] { url = githubHref, label = image [ height (px (Theme.fontSizeH3 deviceClass * 3 // 4)) ] { src = "./assets/icon-github.png", description = "Link to GitHub" } }
                ]
            , el [ paddingXY 24 12 ] <| paragraph [] [ text desc ]
            , column [ paddingEach { top = 12, right = 24, bottom = 24, left = 24 } ] <|
                List.map (\techStackEntry -> paragraph [] [ text <| "➤ " ++ techStackEntry ]) techStack
            ]


viewFooter : Model -> Element msg
viewFooter { theme, deviceClass } =
    let
        largeFontSize : Int
        largeFontSize =
            30

        imgLink : String -> String -> String -> Element msg
        imgLink url src description =
            newTabLink [] { url = url, label = image [ height (px largeFontSize) ] { src = src, description = description } }
    in
    column
        [ width fill
        , paddingXY 48 40
        , spacing 24
        , Background.color theme.primary
        , Font.color theme.background
        ]
        [ paragraph [] [ text "If you want to see something done by me or wish to cooperate in other ways," ]
        , wrappedRow [ spacing 12 ]
            [ el [ Font.size (Theme.fontSizeH2 deviceClass) ] (text "Contact me via: ")
            , row [ spacing 12 ]
                [ imgLink "mailto:kniazevs.v@yandex.ru" "./assets/icon-mail.svg" "E-Mail"
                , imgLink "https://www.linkedin.com/in/sergei-kniazev-394506210/" "./assets/icon-linkedin.png" "LinkedIn"
                , imgLink "https://github.com/Jeaciaz" "./assets/icon-github.png" "GitHub"
                , imgLink "https://t.me/Jeaciaz" "./assets/icon-telegram.svg" "Telegram"
                ]
            ]
        ]


viewCaret : Model -> Element msg
viewCaret { theme } =
    el [ Background.color theme.background, Font.color <| rgba 0 0 0 0, htmlAttribute (Attr.style "user-select" "none") ] <| text "m"


viewH2 : Model -> Element msg -> Element msg
viewH2 { deviceClass } =
    el [ Region.heading 2, Font.size (Theme.fontSizeH2 deviceClass), Font.semiBold, roboto ]


viewH3 : Model -> Element msg -> Element msg
viewH3 { deviceClass } =
    el [ Region.heading 3, Font.size (Theme.fontSizeH3 deviceClass), Font.semiBold, roboto ]


viewLink : Color -> Color -> String -> Element msg
viewLink colorIdle colorHovered label =
    label
        |> text
        |> el
            [ Font.underline, Font.semiBold, Font.color colorIdle, mouseOver [ Font.color colorHovered ], transition [ Transition.color ] ]
