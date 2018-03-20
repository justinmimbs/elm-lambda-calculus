module Tests exposing (..)

import Expect
import Lambda exposing (Expression(..))
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
