module Interactive exposing (main)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Lambda exposing (Expression)
import Parser exposing ((|.), (|=), Parser)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = defaultModel
        , view = view
        , update = update
        }


defaultModel : Model
defaultModel =
    Model
        """S = \\nsz.s(nsz)
0 = \\sz.z
1 = \\sz.sz
2 = \\sz.s(sz)
3 = \\sz.s(s(sz))
6 = \\sz.s(s(s(s(s(sz)))))
+ = \\xy.xSy
* = \\xys.x(ys)
"""
        "*2(+21)"


type alias Model =
    { definitions : String
    , expression : String
    }


type Msg
    = UpdateDefinitions String
    | UpdateExpression String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateDefinitions s ->
            { model | definitions = s }

        UpdateExpression s ->
            { model | expression = s }


view : Model -> Html Msg
view { definitions, expression } =
    let
        definitionList =
            definitions
                |> String.lines
                |> List.filterMap
                    (Parser.run (Parser.map2 always Lambda.parseDefinition Parser.end) >> Result.toMaybe)

        expressionResult =
            expression
                |> Parser.run (Parser.map2 always Lambda.parseExpression Parser.end)
                |> Result.map (Lambda.include definitionList >> Lambda.eval >> Lambda.simplify definitionList)
    in
    Html.div
        []
        [ Html.node "style" [] [ Html.text "@import url(./style.css);" ]
        , Html.textarea
            [ Html.Attributes.value definitions
            , Html.Events.onInput UpdateDefinitions
            , Html.Attributes.spellcheck False
            ]
            []
        , Html.input
            [ Html.Attributes.value expression
            , Html.Events.onInput UpdateExpression
            , Html.Attributes.spellcheck False
            , Html.Attributes.classList
                [ ( "error", expressionResult |> isErr )
                ]
            ]
            []
        , Html.div
            []
            [ Html.text (expressionResult |> unwrap toString Lambda.print)
            ]
        ]



-- result


isErr : Result x a -> Bool
isErr result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


unwrap : (x -> b) -> (a -> b) -> Result x a -> b
unwrap fromErr fromOk result =
    case result of
        Err x ->
            fromErr x

        Ok a ->
            fromOk a
