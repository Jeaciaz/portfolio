port module Main exposing (..)

import Browser
import Browser.Events as BrowserEvent
import Element exposing (..)
import Element.Background as Background
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



-- MODEL


typewrittenTexts : List (List TypewriterBlock)
typewrittenTexts =
    [ [ Text "Hello there!" ]
    , [ Text "My name is Sergei Knyazev. I am a passionate frontend developer who loves delivering top-quality web applications and takes pride in his work." ]
    , [ Text "I primarily work in React and Typescript, but outside of my full-time job I love using functional languages with sound type systems, such as Elm or Haskell. In fact, this very website has been "
      , Link "https://github.com" "written in Elm!"
      ]
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
    , Cmd.none
    )



-- UPDATE


type Msg
    = TypewriterTick
    | SpeedUp
    | UpdateViewportHeight Int



-- UPDATE


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
                scrollToID recentWorkId

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


noopAttr =
    rotate 0


recentWorkId =
    "recent-work"



-- | example use: el [ transition [ Transition.color, Transition.backgroundColor ] ]


transition : List (Transition.Millis -> List Transition.Option -> Transition.Property) -> Attribute Msg
transition =
    Transition.all { duration = 100, options = [ Transition.easeInOut ] } >> htmlAttribute


view : Model -> Html Msg
view model =
    layout [ montserrat, Background.color model.theme.background, Font.color model.theme.text ] <|
        column
            [ width fill
            , height (px model.viewportHeight)
            , if model.isTypewritingComplete then
                noopAttr

              else
                clip
            ]
            [ terminal model
            , column [ htmlAttribute (Attr.id recentWorkId), Background.color model.theme.hyperlink, height (px model.viewportHeight), width fill ] []
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
                                    |> List.map (renderTypewriterFragment model)
                                 )
                                    ++ [ if Tuple.first ttext < (fragmentListLength <| Tuple.second ttext) then
                                            caret model

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
                        , caret model
                        ]
                   ]
        ]


renderTypewriterFragment : Model -> TypewriterBlock -> Element Msg
renderTypewriterFragment model frag =
    case frag of
        Text t ->
            text t

        Link href t ->
            newTabLink [] { url = href, label = a model t }


caret : Model -> Element Msg
caret model =
    el [ Background.color model.theme.background, Font.color <| rgba 0 0 0 0, htmlAttribute (Attr.style "user-select" "none") ] <| text "m"


h1 : Model -> Element Msg -> Element Msg
h1 _ =
    el [ Region.heading 1, Font.size 30, Font.semiBold, roboto ]


h2 : Model -> Element Msg -> Element Msg
h2 _ =
    el [ Region.heading 2, Font.size 24, Font.semiBold, roboto ]


a : Model -> String -> Element Msg
a model label =
    label
        |> text
        |> el
            [ Font.underline, Font.semiBold, mouseOver [ Font.color model.theme.secondary ], transition [ Transition.color ] ]
