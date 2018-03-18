module Tests exposing (..)

import Expect
import Lambda exposing (Expression(..))
import Test exposing (Test, describe, test)


test_eval : Test
test_eval =
    describe "eval" <|
        List.map
            (\( a, b ) ->
                test (Lambda.print a) <|
                    \() ->
                        a |> Lambda.eval |> Expect.equal b
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
