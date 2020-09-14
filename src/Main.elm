module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, text)
import Html.Attributes as Attr
import Html.Events
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer
import String.Extra as String


type Formatter
    = Camel
    | Sentence


formatConfig formatter =
    case formatter of
        Camel ->
            { identifier = "camel"
            , function = String.camelize
            }

        Sentence ->
            { identifier = "sentence"
            , function = String.toSentenceCase
            }


extendedHtmlRenderer : Markdown.Renderer.Renderer (Html msg)
extendedHtmlRenderer =
    let
        defaultRenderer =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { defaultRenderer
        | html =
            Markdown.Html.oneOf
                (List.map renderItem [ Camel, Sentence ])
    }


renderItem : Formatter -> Markdown.Html.Renderer (a -> Html msg)
renderItem formatter =
    Markdown.Html.tag (formatConfig formatter |> .identifier)
        (\inputString _ ->
            Html.text ((formatConfig formatter |> .function) inputString)
        )
        |> Markdown.Html.withAttribute "input-string"


view : String -> Html Msg
view markdownInput =
    Html.div [ Attr.style "padding" "20px" ]
        [ markdownInputView markdownInput
        , case
            markdownInput
                |> String.replace "{{camelCase" "<camel input-string="
                |> String.replace "}}" " />"
                |> Markdown.parse
                |> Result.mapError deadEndsToString
                |> Result.andThen (\ast -> Markdown.Renderer.render extendedHtmlRenderer ast)
          of
            Ok rendered ->
                div [] rendered

            Err errors ->
                text errors
        ]


markdownInputView : String -> Html Msg
markdownInputView markdownInput =
    Html.textarea
        [ Attr.value markdownInput
        , Html.Events.onInput OnMarkdownInput
        , Attr.style "width" "100%"
        , Attr.style "height" "500px"
        , Attr.style "font-size" "18px"
        ]
        []


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"


markdownBody =
    """

I can put format strings in the middle like this {{camelCase "case workflow" }} and it gets rendered


"""


type Msg
    = OnMarkdownInput String


type alias Flags =
    ()


type alias Model =
    String


main : Platform.Program Flags Model Msg
main =
    Browser.document
        { init = \flags -> ( markdownBody, Cmd.none )
        , view = \model -> { body = [ view model ], title = "Markdown Example" }
        , update = update
        , subscriptions = \model -> Sub.none
        }


update msg model =
    case msg of
        OnMarkdownInput newMarkdown ->
            ( newMarkdown, Cmd.none )
