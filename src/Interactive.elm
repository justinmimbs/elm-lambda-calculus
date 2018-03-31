module Interactive exposing (main)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Lambda exposing (Definition, Expression)
import Parser exposing ((|.), (|=), Parser)


main : Program Never String String
main =
    Html.beginnerProgram
        { model =
            [ "S = \\nsz.s(nsz)"
            , "0 = \\sz.z"
            , "1 = S0"
            , "2 = S1"
            , "3 = S2"
            , "4 = S3"
            , "5 = S4"
            , "6 = S5"
            , "+ = \\xy.xSy"
            , "* = \\xys.x(ys)"
            , ""
            , "3S"
            , "*2(+21)"
            ]
                |> String.join "\n"
        , view =
            \string ->
                Html.div
                    []
                    [ Html.node "style" [] [ Html.text "@import url(./style.css);" ]
                    , view string
                    ]
        , update = always
        }


type InputLine
    = Def Definition
    | Exp Expression


parseInputLine : Parser InputLine
parseInputLine =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.map Def Lambda.parseDefinition
            , Parser.map Exp Lambda.parseExpression
            ]
        |. Parser.end


type OutputLine
    = Blank
    | Error Parser.Error
    | DefLine Expression
    | ExpLine Expression


accumulateLine : String -> ( List Definition, List OutputLine ) -> ( List Definition, List OutputLine )
accumulateLine line ( defs, output ) =
    if line == "" then
        ( defs, Blank :: output )
    else
        case Parser.run parseInputLine line of
            Err error ->
                ( defs, Error error :: output )

            Ok (Def ( name, exp )) ->
                let
                    evaluated =
                        exp |> Lambda.include defs |> Lambda.eval
                in
                ( ( name, evaluated ) :: defs, DefLine evaluated :: output )

            Ok (Exp exp) ->
                ( defs, ExpLine (exp |> Lambda.include defs |> Lambda.eval |> Lambda.simplify defs) :: output )


viewOutputLine : OutputLine -> Html a
viewOutputLine line =
    case line of
        Blank ->
            Html.div [] []

        Error error ->
            let
                message =
                    case error.problem of
                        Parser.ExpectingSymbol s ->
                            "Expecting \"" ++ s ++ "\""

                        Parser.ExpectingEnd ->
                            "Expecting end, not \"" ++ String.dropLeft (error.col - 1) error.source ++ "\""

                        Parser.BadRepeat ->
                            "Expecting a name"

                        Parser.BadOneOf _ ->
                            "Expecting an expression"

                        _ ->
                            toString error.problem
            in
            Html.div [ Html.Attributes.class "error" ] [ Html.text message ]

        DefLine exp ->
            Html.div [ Html.Attributes.class "dim" ] [ Html.text (exp |> Lambda.print) ]

        ExpLine exp ->
            Html.div [] [ Html.text (exp |> Lambda.print) ]


view : String -> Html String
view string =
    let
        outputLines =
            string
                |> String.lines
                |> List.foldl accumulateLine ( [], [] )
                |> Tuple.second
                |> List.reverse
    in
    Html.div
        [ Html.Attributes.class "sheet"
        ]
        [ Html.div
            [ Html.Attributes.class "margin" ]
            (outputLines
                |> List.length
                |> List.range 1
                |> List.map (\n -> Html.div [] [ Html.text (toString n) ])
            )
        , Html.textarea
            [ Html.Attributes.class "input"
            , Html.Attributes.value string
            , Html.Attributes.spellcheck False
            , Html.Events.onInput identity
            ]
            []
        , Html.div
            [ Html.Attributes.class "output" ]
            (outputLines |> List.map viewOutputLine)
        ]
