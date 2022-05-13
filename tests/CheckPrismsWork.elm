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


c_Whatever : Relation ( c, b ) reachable wrap -> Relation (Stuff a b c d e f) reachable (Maybe wrap)
c_Whatever =
    makeOneToN_
        "c_Whatever"
        (\fn t ->
            case t of
                Whatever a0 a1 ->
                    Just (fn ( a0, a1 ))

                _ ->
                    Nothing
        )
        (\fn t ->
            case t of
                Whatever a0 a1 ->
                    let
                        apply_c ctor ( t0, t1 ) =
                            ctor t0 t1
                    in
                    apply_c Whatever (fn ( a0, a1 ))

                otherwise ->
                    otherwise
        )


c_Things : Relation a reachable wrap -> Relation (Stuff a b c d e f) reachable (Maybe wrap)
c_Things =
    makeOneToN_
        "c_Things"
        (\fn t ->
            case t of
                Things a0 ->
                    Just (fn a0)

                _ ->
                    Nothing
        )
        (\fn t ->
            case t of
                Things a0 ->
                    Things (fn a0)

                otherwise ->
                    otherwise
        )


c_Other : Relation ( a, c ) reachable wrap -> Relation (Stuff a b c d e f) reachable (Maybe wrap)
c_Other =
    makeOneToN_
        "c_Other"
        (\fn t ->
            case t of
                Other a0 a1 ->
                    Just (fn ( a0, a1 ))

                _ ->
                    Nothing
        )
        (\fn t ->
            case t of
                Other a0 a1 ->
                    let
                        apply_c ctor ( t0, t1 ) =
                            ctor t0 t1
                    in
                    apply_c Other (fn ( a0, a1 ))

                otherwise ->
                    otherwise
        )


c_Everything :
    Relation ( a, ( b, ( c, ( d, ( e, f ) ) ) ) ) reachable wrap
    -> Relation (Stuff a b c d e f) reachable (Maybe wrap)
c_Everything =
    makeOneToN_
        "c_Everything"
        (\fn t ->
            case t of
                Everything a0 a1 a2 a3 a4 a5 ->
                    Just (fn ( a0, ( a1, ( a2, ( a3, ( a4, a5 ) ) ) ) ))

                _ ->
                    Nothing
        )
        (\fn t ->
            case t of
                Everything a0 a1 a2 a3 a4 a5 ->
                    let
                        apply_c ctor ( t0, ( t1, ( t2, ( t3, ( t4, t5 ) ) ) ) ) =
                            ctor t0 t1 t2 t3 t4 t5
                    in
                    apply_c Everything (fn ( a0, ( a1, ( a2, ( a3, ( a4, a5 ) ) ) ) ))

                otherwise ->
                    otherwise
        )


tests : Test
tests =
    describe "Test generated code works"
        [ test "`get c_Things` succeeds"
            (\() ->
                Things "stuff"
                    |> get c_Things
                    |> Expect.equal (Just "stuff")
            )
        , test "`get c_Things` fails"
            (\() ->
                Nada
                    |> get c_Things
                    |> Expect.equal Nothing
            )
        , test "`get c_Other` succeeds"
            (\() ->
                Other 1234 "stuff"
                    |> get c_Other
                    |> Expect.equal (Just ( 1234, "stuff" ))
            )
        , test "`get c_Other` fails"
            (\() ->
                Nada
                    |> get c_Other
                    |> Expect.equal Nothing
            )
        , test "`get c_Whatever` succeeds"
            (\() ->
                Whatever 1234 "stuff"
                    |> get c_Whatever
                    |> Expect.equal (Just ( 1234, "stuff" ))
            )
        , test "`get c_Whatever` fails"
            (\() ->
                Nada
                    |> get c_Whatever
                    |> Expect.equal Nothing
            )
        , test "`get c_Everything` succeeds"
            (\() ->
                Everything 1234 "stuff" True 'c' [] Nothing
                    |> get c_Everything
                    |> Expect.equal (Just ( 1234, ( "stuff", ( True, ( 'c', ( [], Nothing ) ) ) ) ))
            )
        , test "`get c_Everything` fails"
            (\() ->
                Nada
                    |> get c_Everything
                    |> Expect.equal Nothing
            )
        ]
