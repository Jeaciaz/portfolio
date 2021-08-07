port module Main exposing (..)

import Browser
import Browser.Events as BrowserEvent
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Simple.Transition as Transition
import Theme exposing (Theme)
import Time
import Util



-- MAIN


main : Program Int Model Msg
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
      , href = "https://github.com/Jeaciaz/restaurant-list"
      , desc = "Just a regular table with a list of Moscow restaurants recommended by a colleague of mine."
      , techStack =
            [ "HTML + CSS (Bootstrap)"
            , "Vanilla JS"
            ]
      }
    , { title = "Restaurant list"
      , href = "https://github.com/Jeaciaz/restaurant-list"
      , desc = "Just a regular table with a list of Moscow restaurants recommended by a colleague of mine."
      , techStack =
            [ "HTML + CSS (Bootstrap)"
            , "Vanilla JS"
            ]
      }
    ]


type alias Model =
    { theme : Theme.Theme
    , viewportHeight : Int
    , typewriteStatus : List TypewriterFragment
    , typewriterSpeed : TypewriterSpeed
    , isTypewritingComplete : Bool
    }


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
    , desc : String
    , techStack : List String
    }


fragmentLength : TypewriterBlock -> Int
fragmentLength frag =
    case frag of
        Link _ str ->
            String.length str

        Text str ->
            String.length str


fragmentListLength : List TypewriterBlock -> Int
fragmentListLength =
    List.foldl (fragmentLength >> (+)) 0


cutTypewriterText : TypewriterFragment -> List TypewriterBlock
cutTypewriterText ( limit, frags ) =
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


init : Int -> ( Model, Cmd msg )
init height =
    ( Model Theme.light height (List.map (\p -> ( 0, p )) typewrittenTexts) Normal False
    , toggleScrollLock True
    )



-- UPDATE


type Msg
    = TypewriterTick
    | SpeedUp
    | UpdateViewportHeight Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypewriterTick ->
            let
                -- separate completed paragraphs and still in progress
                ( completed, uncompleted ) =
                    List.partition (\( len, p ) -> len >= fragmentListLength p) model.typewriteStatus

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
                            len == fragmentListLength p

                        _ ->
                            False
            in
            ( { model
                | typewriteStatus =
                    completed ++ updatedUncompleted
                , isTypewritingComplete = isDone
              }
            , Cmd.none
            )

        SpeedUp ->
            let
                newModel =
                    case model.typewriterSpeed of
                        Normal ->
                            { model | typewriterSpeed = Fast }

                        Fast ->
                            { model | typewriterSpeed = Fastest }

                        Fastest ->
                            { model | typewriteStatus = model.typewriteStatus |> List.map (\( _, p ) -> ( fragmentListLength p, p )), isTypewritingComplete = True }
            in
            ( newModel
            , if newModel.isTypewritingComplete then
                Cmd.batch
                    [ scrollToID recentWorkId
                    , toggleScrollLock False
                    ]

              else
                Cmd.none
            )

        UpdateViewportHeight newHeight ->
            ( { model | viewportHeight = newHeight }, Cmd.none )



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
        [ if not model.isTypewritingComplete then
            Time.every speedInMs (\_ -> TypewriterTick)

          else
            Sub.none
        , BrowserEvent.onKeyPress (Decode.succeed SpeedUp)
        , BrowserEvent.onResize (\_ -> UpdateViewportHeight)
        ]



-- VIEW


montserrat : Attribute msg
montserrat =
    Font.family [ Font.typeface "Montserrat", Font.sansSerif ]


roboto : Attribute msg
roboto =
    Font.family [ Font.typeface "Roboto", Font.monospace ]


noopAttr : Attr decorative msg
noopAttr =
    rotate 0


recentWorkId : String
recentWorkId =
    "recent-work"



-- | example use: el [ transition [ Transition.color, Transition.backgroundColor ] ]


transition : List (Transition.Millis -> List Transition.Option -> Transition.Property) -> Attribute msg
transition =
    Transition.all { duration = 100, options = [ Transition.easeInOut ] } >> htmlAttribute


view : Model -> Html Msg
view model =
    layout [ montserrat, Background.color model.theme.background, Font.color model.theme.text ] <|
        column
            [ width fill
            , if model.isTypewritingComplete then
                noopAttr

              else
                clip
            ]
            [ terminal model
            , column [ htmlAttribute (Attr.id recentWorkId), Background.color model.theme.background, height (px model.viewportHeight), width fill ]
                [ column
                    [ alignTop
                    , centerX
                    , paddingEach { top = 80, right = 0, bottom = 0, left = 0 }
                    , spacing 12
                    ]
                    [ el
                        [ Font.color model.theme.primary
                        , centerX
                        ]
                        (h2 model.theme (text "My recent work"))
                    , paragraph [] [ text "My GitHub doesn’t have much activity due to my work being mostly on my day job. These are things I made in my free time that I don’t consider obsolete." ]
                    ]
                , row [ centerX, centerY, spacing 48 ] <| List.map (card model.theme) cards
                , footer model.theme
                ]
            ]


terminal : Model -> Element Msg
terminal model =
    column
        [ width fill
        , height <| px model.viewportHeight
        , centerX
        , padding 24
        , Font.color model.theme.background
        , Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
        , Font.bold
        , Background.color model.theme.text
        ]
        [ column [ centerX, centerY, width (fill |> maximum 800), height (fill |> maximum 600), spacing 24 ] <|
            List.filterMap
                (\ttext ->
                    if Tuple.first ttext /= 0 then
                        Just <|
                            paragraph []
                                ((ttext
                                    |> cutTypewriterText
                                    |> List.map (renderTypewriterFragment model.theme)
                                 )
                                    ++ [ if Tuple.first ttext < (fragmentListLength <| Tuple.second ttext) then
                                            caret model.theme

                                         else
                                            none
                                       ]
                                )

                    else
                        Nothing
                )
                model.typewriteStatus
                ++ [ paragraph [ paddingXY 0 24 ]
                        [ text <|
                            "Press any key to "
                                ++ (if model.isTypewritingComplete then
                                        "continue"

                                    else
                                        case model.typewriterSpeed of
                                            Normal ->
                                                "speed up"

                                            Fast ->
                                                "speed up again"

                                            Fastest ->
                                                "skip"
                                   )
                        , caret model.theme
                        ]
                   ]
        ]


renderTypewriterFragment : Theme -> TypewriterBlock -> Element Msg
renderTypewriterFragment theme frag =
    case frag of
        Text t ->
            text t

        Link href t ->
            newTabLink [] { url = href, label = a theme.secondary theme.hyperlink t }


card : Theme -> CardInfo -> Element Msg
card theme { title, href, desc, techStack } =
    column
        [ width (px 332)
        , Background.color theme.white
        , Border.rounded 8
        , Border.shadow { offset = ( 2.0, 2.0 ), size = 1.0, blur = 4.0, color = theme.text |> Theme.addOpacity 0.5 }
        ]
        [ el
            [ width fill
            , paddingEach { top = 24, right = 24, bottom = 12, left = 24 }
            , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
            , Border.color theme.background
            ]
          <|
            newTabLink
                [ centerX
                , Font.size 24
                , Font.semiBold
                , montserrat
                ]
                { url = href, label = a theme.hyperlink theme.secondary title }
        , el [ paddingXY 24 12 ] <| paragraph [] [ text desc ]
        , column [ paddingEach { top = 12, right = 24, bottom = 24, left = 24 } ] <|
            List.map (\techStackEntry -> paragraph [] [ text <| "➤ " ++ techStackEntry ]) techStack
        ]


footer : Theme -> Element msg
footer theme =
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
        [ text "If you want to see something done by me or wish to cooperate in other ways,"
        , row [ spacing 12 ]
            [ el [ Font.size 30 ] (text "Contact me via: ")
            , imgLink "mailto:kniazevs.v@yandex.ru" "/assets/icon-mail.svg" "E-Mail"
            , imgLink "https://linkedin.com" "/assets/icon-linkedin.png" "LinkedIn"
            , imgLink "https://github.com/Jeaciaz" "/assets/icon-github.png" "GitHub"
            , imgLink "https://t.me/Jeaciaz" "/assets/icon-telegram.svg" "Telegram"
            ]
        ]


caret : Theme -> Element msg
caret theme =
    el [ Background.color theme.background, Font.color <| rgba 0 0 0 0, htmlAttribute (Attr.style "user-select" "none") ] <| text "m"


h2 : Theme -> Element msg -> Element msg
h2 _ =
    el [ Region.heading 2, Font.size 30, Font.semiBold, roboto ]


h3 : Theme -> Element msg -> Element msg
h3 _ =
    el [ Region.heading 3, Font.size 24, Font.semiBold, roboto ]


a : Color -> Color -> String -> Element msg
a colorIdle colorHovered label =
    label
        |> text
        |> el
            [ Font.underline, Font.semiBold, Font.color colorIdle, mouseOver [ Font.color colorHovered ], transition [ Transition.color ] ]
