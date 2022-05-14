module CheckPrismsWork exposing (tests)

import Accessors exposing (Relation, get, makeOneToN_)
import Expect
import Test exposing (Test, describe, test)


type Stuff a b c d e f
    = Things a
    | Other a c
    | Whatever c b
    | Everything a b c d e f
    | Nada


tests : Test
tests =
    describe "Test generated code works"
        [ test "`get variantThings` succeeds"
            (\() ->
                Things "stuff"
                    |> get variantThings
                    |> Expect.equal (Just "stuff")
            )
        , test "`get variantThings` fails"
            (\() ->
                Nada
                    |> get variantThings
                    |> Expect.equal Nothing
            )
        , test "`get variantOther` succeeds"
            (\() ->
                Other 1234 "stuff"
                    |> get variantOther
                    |> Expect.equal (Just ( 1234, "stuff" ))
            )
        , test "`get variantOther` fails"
            (\() ->
                Nada
                    |> get variantOther
                    |> Expect.equal Nothing
            )
        , test "`get variantWhatever` succeeds"
            (\() ->
                Whatever 1234 "stuff"
                    |> get variantWhatever
                    |> Expect.equal (Just ( 1234, "stuff" ))
            )
        , test "`get variantWhatever` fails"
            (\() ->
                Nada
                    |> get variantWhatever
                    |> Expect.equal Nothing
            )
        , test "`get variantEverything` succeeds"
            (\() ->
                Everything 1234 "stuff" True 'c' [] Nothing
                    |> get variantEverything
                    |> Expect.equal (Just ( 1234, ( "stuff", ( True, ( 'c', ( [], Nothing ) ) ) ) ))
            )
        , test "`get variantEverything` fails"
            (\() ->
                Nada
                    |> get variantEverything
                    |> Expect.equal Nothing
            )
        ]
