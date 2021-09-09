module CodeGen.Test exposing (tests)

import Elm.CodeGen exposing (accessFun)
import Elm.Pretty exposing (prettyExpression)
import Expect
import Pretty exposing (pretty)
import Test exposing (Test, test)


tests : Test
tests =
    test "accessFun"
        (\() ->
            accessFun "field"
                |> prettyExpression
                |> pretty 120
                -- this is wrong
                -- see https://github.com/the-sett/elm-syntax-dsl/issues/33
                |> Expect.equal "field"
        )
