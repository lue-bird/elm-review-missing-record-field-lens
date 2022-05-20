module VariantPrism.GenerateUsed.Test exposing (all)

import Expect
import Fuzz
import Parser
import Review.Test
import Test exposing (Test, describe, test)
import VariantPrism.GenerateUsed exposing (accessors, rule)
import VariantPrism.GenerateUsed.Testable exposing (generationModuleParse)


all : Test
all =
    describe "NoMissingVariantPrism"
        [ Test.fuzz
            Fuzz.string
            "name parser"
            (\baseModule ->
                (baseModule ++ ".Variant")
                    |> Parser.run generationModuleParse
                    |> Expect.equal (Ok { baseModule = baseModule })
            )
        , describe "should generate" shouldGenerate
        , describe "should not generate" dontGenerate
        ]


shouldGenerate : List Test
shouldGenerate =
    [ test "multiple variant values, generation module exposing (..)"
        (\() ->
            [ """module Data.Variant exposing (..)
"""
            , """module Use exposing (..)

using = Data.Variant.some

"""
            , """module Data exposing (Data(..))

type Data a b c d
    = Some a b c d
    | None
"""
            ]
                |> Review.Test.runOnModules (rule { generator = accessors })
                |> Review.Test.expectErrorsForModules
                    [ ( "Use"
                      , [ Review.Test.error
                            { message = "missing `import Data.Variant`"
                            , details =
                                [ "Add the variant prism generation `module` `import` through the supplied fix" ]
                            , under = "Data.Variant.some"
                            }
                            |> Review.Test.whenFixed
                                """module Use exposing (..)

import Data.Variant
using = Data.Variant.some

"""
                        ]
                      )
                    , ( "Data.Variant"
                      , [ Review.Test.error
                            { message = "missing prism for variant `Some`"
                            , details =
                                [ "A variant prism with this name is used in other `module`s."
                                , "Add the generated prism declaration through the fix."
                                ]
                            , under = "(..)"
                            }
                            |> Review.Test.whenFixed
                                """module Data.Variant exposing (some)

import Accessors exposing (makeOneToN_)
import Data exposing (Data(..))

some :
    Relation ( a, ( b, ( c, d ) ) ) reachable wrap
    -> Relation (Data a b c d) reachable (Maybe wrap)
some =
    makeOneToN_
        "Data.Some"
        (\\alterValues variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    ( value0, ( value1, ( value2, value3 ) ) ) |> alterValues |> Just

                _ ->
                    Nothing
        )
        (\\alterValues variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    let
                        ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                            ( value0, ( value1, ( value2, value3 ) ) ) |> alterValues
                    in
                    Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

                someNot ->
                    someNot
        )"""
                        ]
                      )
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
                |> Review.Test.run (rule { generator = accessors })
                |> Review.Test.expectNoErrors
        )
    , test "generate Prism for variants that already have a prism defined."
        (\() ->
            """module A exposing (..)

type AlreadyDefined a
    = DontError a
    | Whatever

variantDontError : Prism (AlreadyDefined a) a
variantDontError =
    makeOneToOne "variantDontError"
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
                |> Review.Test.run (rule { generator = accessors })
                |> Review.Test.expectNoErrors
        )
    ]