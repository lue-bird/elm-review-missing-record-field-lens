module NoMissingVariantPrism.Test exposing (all)

import NoMissingVariantPrism exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingVariantPrism"
        [ describe "should generate" shouldGenerate
        , describe "should not generate" dontGenerate
        ]


shouldGenerate : List Test
shouldGenerate =
    [ test "Prism for types with more than one type variable and tuple up arguments given to the same variant"
        (\() ->
            """module A exposing (..)

type Data3 a b c d
    = Data3_Wat a b c d
    | Data3_Otherwise
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Generating a `c_Data3_Wat` Prism for the type variant: `Data3_Wat`."
                        , details = [ "missing prism for variant `Data3_Wat`" ]
                        , under = "Data3_Wat a b c d"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)

type Data3 a b c d
    = Data3_Wat a b c d
    | Data3_Otherwise



c_Data3_Wat :
    Relation ( a, ( b, ( c, d ) ) ) reachable wrap
    -> Relation (Data3 a b c d) reachable (Maybe wrap)
c_Data3_Wat =
    makeOneToN
        "c_Data3_Wat"
        (\\fn t ->
            case t of
                Data3_Wat a0 a1 a2 a3 ->
                    Just (fn ( a0, ( a1, ( a2, a3 ) ) ))

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data3_Wat a0 a1 a2 a3 ->
                    let
                        apply_c ctor ( t0, ( t1, ( t2, t3 ) ) ) =
                            ctor t0 t1 t2 t3
                    in
                    apply_c Data3_Wat (fn ( a0, ( a1, ( a2, a3 ) ) ))

                otherwise ->
                    otherwise
        )"""
                    ]
        )
    , test "Prisms for types with more than one type variable"
        (\() ->
            """module A exposing (..)



type Data2 a b
    = Data2_Things a
    | Data2_Blah b
    | Data2_Otherwise
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Generating a `c_Data2_Things` Prism for the type variant: `Data2_Things`."
                        , details = [ "missing prism for variant `Data2_Things`" ]
                        , under = "Data2_Things a"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)



type Data2 a b
    = Data2_Things a
    | Data2_Blah b
    | Data2_Otherwise



c_Data2_Things : Relation a reachable wrap -> Relation (Data2 a b) reachable (Maybe wrap)
c_Data2_Things =
    makeOneToN
        "c_Data2_Things"
        (\\fn t ->
            case t of
                Data2_Things a0 ->
                    Just (fn a0)

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data2_Things a0 ->
                    Data2_Things (fn a0)

                otherwise ->
                    otherwise
        )"""
                    , Review.Test.error
                        { message = "Generating a `c_Data2_Blah` Prism for the type variant: `Data2_Blah`."
                        , details = [ "missing prism for variant `Data2_Blah`" ]
                        , under = "Data2_Blah b"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)



type Data2 a b
    = Data2_Things a
    | Data2_Blah b
    | Data2_Otherwise



c_Data2_Blah : Relation b reachable wrap -> Relation (Data2 a b) reachable (Maybe wrap)
c_Data2_Blah =
    makeOneToN
        "c_Data2_Blah"
        (\\fn t ->
            case t of
                Data2_Blah a0 ->
                    Just (fn a0)

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data2_Blah a0 ->
                    Data2_Blah (fn a0)

                otherwise ->
                    otherwise
        )"""
                    ]
        )
    , test "a basic prism"
        (\() ->
            """module A exposing (..)


type Data1 a
    = Data1_Stuff a
    | Data1_Otherwise
"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Generating a `c_Data1_Stuff` Prism for the type variant: `Data1_Stuff`."
                        , details = [ "missing prism for variant `Data1_Stuff`" ]
                        , under = "Data1_Stuff a"
                        }
                        |> Review.Test.whenFixed
                            """module A exposing (..)


type Data1 a
    = Data1_Stuff a
    | Data1_Otherwise



c_Data1_Stuff : Relation a reachable wrap -> Relation (Data1 a) reachable (Maybe wrap)
c_Data1_Stuff =
    makeOneToN
        "c_Data1_Stuff"
        (\\fn t ->
            case t of
                Data1_Stuff a0 ->
                    Just (fn a0)

                _ ->
                    Nothing
        )
        (\\fn t ->
            case t of
                Data1_Stuff a0 ->
                    Data1_Stuff (fn a0)

                otherwise ->
                    otherwise
        )"""
                    ]
        )
    ]


dontGenerate : List Test
dontGenerate =
    [ test "attempt to fix Lens-able types"
        (\() ->
            """module A exposing (..)


type NotAVariant =
    NotAVariant String
"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
        )
    , test "generate Prism for variants that already have a prism defined."
        (\() ->
            """module A exposing (..)

type AlreadyDefined a
    = DontError a
    | Whatever

c_DontError : Prism (AlreadyDefined a) a
c_DontError =
    makeOneToOne "c_DontError"
        (\\t ->
            case t of
                DontError a -> Just a
                otherwise -> Nothing
            )
        (\\fn t ->
            case t of
                DontError a -> DontError (fn a)
                otherwise -> otherwise
        )


"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
        )
    ]
