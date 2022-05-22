module VariantPrism.GenerateUsed.Test exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import VariantPrism.GenerateUsed


all : Test
all =
    describe "NoMissingVariantPrism"
        [ generates
        , dontGenerate
        ]


generates : Test
generates =
    describe "generates"
        [ test "multiple variant values, generation module exposing (..)"
            (\() ->
                [ """module Data.Extra.Local exposing (..)
"""
                , """module Use exposing (use)

use = Data.onSome

"""
                , """module Data exposing (Data(..))

type Data a b c d
    = Some a b c d
    | None
"""
                ]
                    |> Review.Test.runOnModules
                        ({ name = VariantPrism.GenerateUsed.prismNameOnVariant
                         , build = VariantPrism.GenerateUsed.accessors
                         }
                            |> VariantPrism.GenerateUsed.inVariantOriginModuleDotSuffix
                                "Extra.Local"
                            |> VariantPrism.GenerateUsed.importGenerationModuleAsOriginModule
                            |> VariantPrism.GenerateUsed.rule
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Use"
                          , [ Review.Test.error
                                { message = "missing `import Data.Extra.Local`"
                                , details =
                                    [ "Add the variant prism generation `module` `import` through the supplied fix" ]
                                , under = "Data.onSome"
                                }
                                |> Review.Test.whenFixed
                                    """module Use exposing (use)

import Data.Extra.Local as Data
use = Data.onSome

"""
                            ]
                          )
                        , ( "Data.Extra.Local"
                          , [ Review.Test.error
                                { message = "missing prism for variant `Some`"
                                , details =
                                    [ "A variant prism with this name is used in other `module`s."
                                    , "Add the generated prism declaration through the fix."
                                    ]
                                , under = "(..)"
                                }
                                |> Review.Test.whenFixed
                                    """module Data.Extra.Local exposing (some)

import Accessors exposing (makeOneToN_)
import Data exposing (Data(..))

some :
    Relation ( a, ( b, ( c, d ) ) ) reachable wrap
    -> Relation (Data a b c d) reachable (Maybe wrap)
some =
    makeOneToN_
        "Data.Some"
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    let
                        ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                            ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter
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


dontGenerate : Test
dontGenerate =
    describe "doesn't generate"
        [ test "attempt to fix Lens-able types"
            (\() ->
                [ """module Data exposing (One (..))

type One
    = One String
"""
                , """module Use exposing (use)

use = Data.onOne
"""
                ]
                    |> Review.Test.runOnModules
                        ({ name = VariantPrism.GenerateUsed.prismNameOnVariant
                         , build = VariantPrism.GenerateUsed.accessors
                         }
                            |> VariantPrism.GenerateUsed.inVariantOriginModuleDotSuffix
                                "Extra.Local"
                            |> VariantPrism.GenerateUsed.importGenerationModuleAsOriginModule
                            |> VariantPrism.GenerateUsed.rule
                        )
                    |> Review.Test.expectNoErrors
            )
        , test "generate Prism for variants that already have a prism defined."
            (\() ->
                [ """module Data exposing (AlreadyDefined(..))

type AlreadyDefined a
    = DontError a
    | Whatever

variantDontError : Prism (AlreadyDefined a) a
variantDontError = ()
"""
                , """module Use exposing (use)

use = Data.onOne
"""
                ]
                    |> Review.Test.runOnModules
                        ({ name = VariantPrism.GenerateUsed.prismNameOnVariant
                         , build = VariantPrism.GenerateUsed.accessors
                         }
                            |> VariantPrism.GenerateUsed.inVariantOriginModuleDotSuffix
                                "Extra.Local"
                            |> VariantPrism.GenerateUsed.importGenerationModuleAsOriginModule
                            |> VariantPrism.GenerateUsed.rule
                        )
                    |> Review.Test.expectNoErrors
            )
        ]
