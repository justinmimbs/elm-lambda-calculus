module Tests exposing (..)

import Expect
import Lambda exposing (Definition, Expression(..))
import Parser
import Test exposing (Test, describe, test)


fn =
    Function


ap =
    Application


n =
    Name


test_eval : Test
test_eval =
    let
        toTest : ( Expression, Expression ) -> Test
        toTest ( expression, expected ) =
            test (Lambda.print expression) <|
                \() ->
                    expression |> Lambda.eval |> Expect.equal expected
    in
    describe "eval"
        [ describe "reduces an expression as expected" <|
            List.map toTest
                [ ( ap (fn 'x' (n 'x')) (n 'y')
                  , n 'y'
                  )
                , ( ap (fn 'x' (fn 'y' (ap (n 'x') (n 'y')))) (n 'y')
                  , fn 'a' (ap (n 'y') (n 'a'))
                  )
                , ( ap
                        (fn 'x' (fn 'y' (ap (n 'x') (fn 'x' (ap (n 'x') (n 'y'))))))
                        (n 'y')
                  , fn 'a' (ap (n 'y') (fn 'x' (ap (n 'x') (n 'a'))))
                  )
                ]
        , describe "reduces in normal order" <|
            List.map toTest
                [ ( ap
                        (fn 'x' (n 'y'))
                        (ap (fn 'x' (ap (n 'x') (n 'x'))) (fn 'x' (ap (n 'x') (n 'x'))))
                  , n 'y'
                  )
                ]
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
    describe "print"
        [ describe "prints names and functions" <|
            List.map toTest
                [ ( n 'x'
                  , "x"
                  )
                , ( fn 'x' (n 'x')
                  , "\\x.x"
                  )
                ]
        , describe "uses parentheses to disambiguate"
            [ describe "function application" <|
                List.map toTest
                    [ ( fn 'x' (ap (n 'x') (n 'y'))
                      , "\\x.xy"
                      )
                    , ( ap (fn 'x' (n 'x')) (n 'y')
                      , "(\\x.x)y"
                      )
                    , ( ap (fn 'x' (n 'x')) (fn 'y' (n 'y'))
                      , "(\\x.x)(\\y.y)"
                      )
                    , ( ap (n 'x') (fn 'y' (n 'y'))
                      , "x(\\y.y)"
                      )
                    ]
            , describe "right-nested application" <|
                List.map toTest
                    [ ( ap (ap (ap (n 'a') (n 'b')) (n 'c')) (n 'd')
                      , "abcd"
                      )
                    , ( ap (n 'a') (ap (n 'b') (ap (n 'c') (n 'd')))
                      , "a(b(cd))"
                      )
                    ]
            , describe "right-nested function application" <|
                List.map toTest
                    [ ( ap
                            (ap (fn 'x' (n 'x')) (fn 'y' (n 'y')))
                            (n 'z')
                      , "(\\x.x)(\\y.y)z"
                      )
                    , ( ap
                            (fn 'x' (n 'x'))
                            (ap (fn 'y' (n 'y')) (n 'z'))
                      , "(\\x.x)((\\y.y)z)"
                      )
                    ]
            ]
        , describe "abbreviates a sequence of functions as a single uncurried function" <|
            List.map toTest
                [ ( fn 'x' (fn 'y' (n 'x'))
                  , "\\xy.x"
                  )
                , ( fn 'x' (fn 'y' (fn 'z' (n 'x')))
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
        [ describe "converts a printed expression back to its original form" <|
            List.map toTest
                [ n 'x'
                , fn 'x' (n 'x')
                , fn 'x' (ap (n 'x') (n 'y'))
                , ap (fn 'x' (n 'x')) (n 'y')
                , ap (fn 'x' (n 'x')) (fn 'y' (n 'y'))
                , ap (n 'x') (fn 'y' (n 'y'))
                , ap (ap (ap (n 'a') (n 'b')) (n 'c')) (n 'd')
                , ap (n 'a') (ap (n 'b') (ap (n 'c') (n 'd')))
                , ap
                    (ap (fn 'x' (n 'x')) (fn 'y' (n 'y')))
                    (n 'z')
                , ap
                    (fn 'x' (n 'x'))
                    (ap (fn 'y' (n 'y')) (n 'z'))
                , fn 'x' (fn 'y' (n 'x'))
                , fn 'x' (fn 'y' (fn 'z' (n 'x')))
                ]
        ]


test_include : Test
test_include =
    describe "include"
        [ describe "provides a list of definitions to an expression (as function arguments)"
            [ test "in order" <|
                \() ->
                    Lambda.include
                        [ ( 'A', n 'a' )
                        , ( 'B', n 'b' )
                        , ( 'C', n 'c' )
                        ]
                        (n 'x')
                        |> Expect.equal
                            (ap
                                (fn 'A'
                                    (ap
                                        (fn 'B'
                                            (ap
                                                (fn 'C' (n 'x'))
                                                (n 'c')
                                            )
                                        )
                                        (n 'b')
                                    )
                                )
                                (n 'a')
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
            [ ( [ ( 'I', fn 'x' (n 'x') )
                ]
              , fn 'x' (n 'x')
              , n 'I'
              )
            , ( [ ( 'I', fn 'x' (n 'x') )
                ]
              , ap (fn 'x' (n 'x')) (n 'x')
              , ap (n 'I') (n 'x')
              )
            , ( [ ( 'I', fn 'x' (n 'x') )
                , ( 'T', fn 'x' (fn 'y' (n 'x')) )
                , ( 'F', fn 'x' (fn 'y' (n 'y')) )
                ]
              , ap (ap (fn 'x' (n 'x')) (fn 'x' (fn 'y' (n 'x')))) (fn 'x' (fn 'y' (n 'y')))
              , ap (ap (n 'I') (n 'T')) (n 'F')
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
              , fn 'x' (n 'x')
              )
            , ( 'S'
              , fn 'w' (fn 'y' (fn 'x' (ap (n 'y') (ap (ap (n 'w') (n 'y')) (n 'x')))))
              )
            ]


test_equivalent : Test
test_equivalent =
    let
        toTest : Bool -> ( Expression, Expression ) -> Test
        toTest expected ( x, y ) =
            commutative (Lambda.print x ++ " ≡ " ++ Lambda.print y) Lambda.equivalent x y expected
    in
    describe "equivalent"
        [ describe "returns True when expressions have the same structure and a correspondence exists between names in the same scope" <|
            List.map (toTest True)
                [ ( n 'x'
                  , n 'x'
                  )
                , ( n 'x'
                  , n 'y'
                  )
                , ( ap (n 'x') (n 'y')
                  , ap (n 'y') (n 'x')
                  )
                , ( fn 'x' (fn 'x' (n 'x'))
                  , fn 'x' (fn 'y' (n 'y'))
                  )
                , ( fn 'x' (ap (n 'x') (fn 'x' (n 'x')))
                  , fn 'y' (ap (n 'y') (fn 'z' (n 'z')))
                  )
                ]
        , describe "returns False when expressions have different structures" <|
            List.map (toTest False)
                [ ( n 'x'
                  , fn 'x' (n 'x')
                  )
                , ( fn 'x' (n 'x')
                  , ap (n 'x') (n 'x')
                  )
                , ( ap (n 'x') (n 'x')
                  , n 'x'
                  )
                ]
        , describe "returns False when expressions have the same structure but no correspondence exists between names in the same scope" <|
            List.map (toTest False)
                [ ( ap (n 'x') (n 'x')
                  , ap (n 'x') (n 'y')
                  )
                , ( ap (ap (n 'x') (n 'y')) (ap (n 'x') (n 'y'))
                  , ap (ap (n 'x') (n 'y')) (ap (n 'x') (n 'z'))
                  )
                , ( fn 'x' (fn 'x' (n 'y'))
                  , fn 'x' (fn 'y' (n 'x'))
                  )
                , ( fn 'x' (ap (n 'x') (fn 'x' (n 'z')))
                  , fn 'y' (ap (n 'y') (fn 'z' (n 'z')))
                  )
                ]
        ]


commutative : String -> (a -> a -> b) -> a -> a -> b -> Test
commutative description f x y expected =
    describe description
        [ test "x y" <| \() -> f x y |> Expect.equal expected
        , test "y x" <| \() -> f y x |> Expect.equal expected
        ]
