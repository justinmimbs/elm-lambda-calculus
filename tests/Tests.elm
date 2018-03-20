module Tests exposing (..)

import Expect
import Lambda exposing (Definition, Expression(..))
import Parser
import Test exposing (Test, describe, test)


test_eval : Test
test_eval =
    describe "eval" <|
        List.map
            (\( expression, expected ) ->
                test (Lambda.print expression) <|
                    \() ->
                        expression |> Lambda.eval |> Expect.equal expected
            )
            [ ( Application (Function 'x' (Name 'x')) (Name 'y')
              , Name 'y'
              )
            , ( Application (Function 'x' (Function 'y' (Application (Name 'x') (Name 'y')))) (Name 'y')
              , Function 'a' (Application (Name 'y') (Name 'a'))
              )
            , ( Application
                    (Function 'x' (Function 'y' (Application (Name 'x') (Function 'x' (Application (Name 'x') (Name 'y'))))))
                    (Name 'y')
              , Function 'a' (Application (Name 'y') (Function 'x' (Application (Name 'x') (Name 'a'))))
              )
            ]


test_print : Test
test_print =
    let
        toTest : ( Expression, String ) -> Test
        toTest ( expression, expected ) =
            test expected <|
                \() ->
                    expression |> Lambda.print |> Expect.equal expected
    in
    describe "print" <|
        [ describe "prints names and functions" <|
            List.map toTest
                [ ( Name 'x'
                  , "x"
                  )
                , ( Function 'x' (Name 'x')
                  , "\\x.x"
                  )
                ]
        , describe "uses parentheses to disambiguate" <|
            [ describe "function application" <|
                List.map toTest
                    [ ( Function 'x' (Application (Name 'x') (Name 'y'))
                      , "\\x.xy"
                      )
                    , ( Application (Function 'x' (Name 'x')) (Name 'y')
                      , "(\\x.x)y"
                      )
                    , ( Application (Function 'x' (Name 'x')) (Function 'y' (Name 'y'))
                      , "(\\x.x)(\\y.y)"
                      )
                    , ( Application (Name 'x') (Function 'y' (Name 'y'))
                      , "x(\\y.y)"
                      )
                    ]
            , describe "right-nested application" <|
                List.map toTest
                    [ ( Application (Application (Application (Name 'a') (Name 'b')) (Name 'c')) (Name 'd')
                      , "abcd"
                      )
                    , ( Application (Name 'a') (Application (Name 'b') (Application (Name 'c') (Name 'd')))
                      , "a(b(cd))"
                      )
                    ]
            , describe "right-nested function application" <|
                List.map toTest
                    [ ( Application
                            (Application (Function 'x' (Name 'x')) (Function 'y' (Name 'y')))
                            (Name 'z')
                      , "(\\x.x)(\\y.y)z"
                      )
                    , ( Application
                            (Function 'x' (Name 'x'))
                            (Application (Function 'y' (Name 'y')) (Name 'z'))
                      , "(\\x.x)((\\y.y)z)"
                      )
                    ]
            ]
        , describe "abbreviates a sequence of functions as a single 'multi-argument' function" <|
            List.map toTest
                [ ( Function 'x' (Function 'y' (Name 'x'))
                  , "\\xy.x"
                  )
                , ( Function 'x' (Function 'y' (Function 'z' (Name 'x')))
                  , "\\xyz.x"
                  )
                ]
        ]


test_parseExpression : Test
test_parseExpression =
    let
        toTest : Expression -> Test
        toTest expression =
            test (expression |> Lambda.print) <|
                \() ->
                    expression |> Lambda.print |> Parser.run Lambda.parseExpression |> Expect.equal (Ok expression)
    in
    describe "parseExpression"
        [ describe "converts a printed Expression back to its original form" <|
            List.map toTest
                [ Name 'x'
                , Function 'x' (Name 'x')
                , Function 'x' (Application (Name 'x') (Name 'y'))
                , Application (Function 'x' (Name 'x')) (Name 'y')
                , Application (Function 'x' (Name 'x')) (Function 'y' (Name 'y'))
                , Application (Name 'x') (Function 'y' (Name 'y'))
                , Application (Application (Application (Name 'a') (Name 'b')) (Name 'c')) (Name 'd')
                , Application (Name 'a') (Application (Name 'b') (Application (Name 'c') (Name 'd')))
                , Application
                    (Application (Function 'x' (Name 'x')) (Function 'y' (Name 'y')))
                    (Name 'z')
                , Application
                    (Function 'x' (Name 'x'))
                    (Application (Function 'y' (Name 'y')) (Name 'z'))
                , Function 'x' (Function 'y' (Name 'x'))
                , Function 'x' (Function 'y' (Function 'z' (Name 'x')))
                ]
        ]


test_include : Test
test_include =
    describe "include"
        [ describe "provides a list of definitions to an expression (as function arguments)"
            [ test "in order" <|
                \() ->
                    Lambda.include
                        [ ( 'A', Name 'a' )
                        , ( 'B', Name 'b' )
                        , ( 'C', Name 'c' )
                        ]
                        (Name 'x')
                        |> Expect.equal
                            (Application
                                (Function 'A'
                                    (Application
                                        (Function 'B'
                                            (Application
                                                (Function 'C' (Name 'x'))
                                                (Name 'c')
                                            )
                                        )
                                        (Name 'b')
                                    )
                                )
                                (Name 'a')
                            )
            ]
        ]


test_simplify : Test
test_simplify =
    let
        toTest : ( List Definition, Expression, Expression ) -> Test
        toTest ( defs, expression, expected ) =
            test (Lambda.print expression) <|
                \() ->
                    Lambda.simplify defs expression |> Expect.equal expected
    in
    describe "simplify" <|
        List.map toTest
            [ ( [ ( 'I', Function 'x' (Name 'x') )
                ]
              , Function 'x' (Name 'x')
              , Name 'I'
              )
            , ( [ ( 'I', Function 'x' (Name 'x') )
                ]
              , Application (Function 'x' (Name 'x')) (Name 'x')
              , Application (Name 'I') (Name 'x')
              )
            , ( [ ( 'I', Function 'x' (Name 'x') )
                , ( 'T', Function 'x' (Function 'y' (Name 'x')) )
                , ( 'F', Function 'x' (Function 'y' (Name 'y')) )
                ]
              , Application
                    (Application
                        (Function 'x' (Name 'x'))
                        (Function 'x' (Function 'y' (Name 'x')))
                    )
                    (Function 'x' (Function 'y' (Name 'y')))
              , Application (Application (Name 'I') (Name 'T')) (Name 'F')
              )
            ]


test_parseDefinition : Test
test_parseDefinition =
    let
        toTest : Definition -> List Test
        toTest (( name, expression ) as definition) =
            let
                spaces =
                    String.cons name (" = " ++ Lambda.print expression)

                noSpaces =
                    String.cons name ("=" ++ Lambda.print expression)
            in
            [ test spaces <|
                \() ->
                    spaces |> Parser.run Lambda.parseDefinition |> Expect.equal (Ok definition)
            , test noSpaces <|
                \() ->
                    noSpaces |> Parser.run Lambda.parseDefinition |> Expect.equal (Ok definition)
            ]
    in
    describe "parseDefinition" <|
        List.concatMap toTest
            [ ( 'I'
              , Function 'x' (Name 'x')
              )
            , ( 'S'
              , Function 'w' (Function 'y' (Function 'x' (Application (Name 'y') (Application (Application (Name 'w') (Name 'y')) (Name 'x')))))
              )
            ]
