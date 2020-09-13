module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, div, text)
import Html.Attributes as Attr
import Html.Events
import Markdown.Parser as Markdown
import Markdown.Renderer
import Markdown.Block as Block
import Markdown.Html
import String.Extra as String





defaultHtmlRenderer : Markdown.Renderer.Renderer (Html msg)
defaultHtmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [] children

                Block.H2 ->
                    Html.h2 [] children

                Block.H3 ->
                    Html.h3 [] children

                Block.H4 ->
                    Html.h4 [] children

                Block.H5 ->
                    Html.h5 [] children

                Block.H6 ->
                    Html.h6 [] children
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote []
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href link.destination
                        , Attr.title title
                        ]
                        content

                Nothing ->
                    Html.a [ Attr.href link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = 
        Markdown.Html.oneOf
         [Markdown.Html.tag "camel"
            (\ inputString children->
                Html.text (String.camelize inputString )
            )
            |> Markdown.Html.withAttribute "input-string"
         ]
         
        
    , codeBlock =
        \{ body, language } ->
            Html.pre []
                [ Html.code []
                    [ Html.text body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell = 
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.td attrs
    }
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
                |> Result.andThen (\ast -> Markdown.Renderer.render defaultHtmlRenderer ast)
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
