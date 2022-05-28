module VariantPrism.GenerateUsed.Test exposing (all)

import Elm.CodeGen as CodeGen
import Expect
import Help exposing (declarationToString)
import Review.Test
import Test exposing (Test, describe, test)
import VariantPrism.GenerateUsed
import VariantPrism.GenerateUsed.Testable exposing (prismDeclarationToCodeGen)


all : Test
all =
    describe "NoMissingVariantPrism"
        [ reported
        , accepted
        , build
        ]


build : Test
build =
    Test.describe
        "build"
        [ test
            "accessors"
            (\() ->
                VariantPrism.GenerateUsed.accessors
                    |> declarationBuildTestString
                    |> Expect.equal
                        """{-| Accessor prism for the variant `Data.Some` of the `type Data`.


-}
some : Lens (Data a b c d) transformed ( a, ( b, ( c, d ) ) ) wrap
some =
    makeOneToN_
        "Data.Some"
        (\\variantValuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\variantValuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    let
                        ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                            ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter
                    in
                    Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

                someNot ->
                    someNot
        )"""
            )
        , test
            "accessorsBChiquet"
            (\() ->
                VariantPrism.GenerateUsed.accessorsBChiquet
                    |> declarationBuildTestString
                    |> Expect.equal
                        """{-| Accessor prism for the variant `Data.Some` of the `type Data`.


-}
some :
    Relation ( a, ( b, ( c, d ) ) ) reachable wrap -> Relation (Data a b c d) reachable (Maybe wrap)
some =
    makeOneToN
        (\\variantValuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\variantValuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    let
                        ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                            ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter
                    in
                    Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

                someNot ->
                    someNot
        )"""
            )
        ]


reported : Test
reported =
    Test.describe
        "reported"
        [ test "attempt to fix Lens-able types"
            (\() ->
                [ """module Data exposing (Data(..))

type Data
    = Some String
    | None
"""
                , """module Use exposing (use)

import Data.On


use = Data.On.one
"""
                ]
                    |> Review.Test.runOnModules
                        (VariantPrism.GenerateUsed.rule
                            { build = VariantPrism.GenerateUsed.accessors
                            , name = VariantPrism.GenerateUsed.prismNameVariant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Use"
                          , [ Review.Test.error
                                { message = "variant prism generation `module Data.On` missing"
                                , details =
                                    [ "Create such an elm file where variant prisms will be generated in."
                                    , "At the time of writing, [`elm-review` isn't able to generate new files](https://github.com/jfmengels/elm-review/issues/125)."
                                    ]
                                , under = "Data.On.one"
                                }
                            ]
                          )
                        ]
            )
        , test
            "multiple variant values, generation module exposing (..)"
            (\() ->
                [ """module Data.On exposing (..)
"""
                , """module Use exposing (use)

use = Data.On.some

"""
                , """module Data exposing (Data(..))

type Data a b c d
    = Some a b c d
    | None
"""
                ]
                    |> Review.Test.runOnModules
                        (VariantPrism.GenerateUsed.rule
                            { build = VariantPrism.GenerateUsed.accessors
                            , name = VariantPrism.GenerateUsed.prismNameVariant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Use"
                          , [ Review.Test.error
                                { message = "`import Data.On` missing"
                                , details =
                                    [ "Add the variant prism generation `module` `import` through the supplied fix."
                                    ]
                                , under = "Data.On.some"
                                }
                                |> Review.Test.whenFixed
                                    """module Use exposing (use)

import Data.On
use = Data.On.some

"""
                            ]
                          )
                        , ( "Data.On"
                          , [ Review.Test.error
                                { message = "variant prism on `Data.Some` missing"
                                , details =
                                    [ "A variant prism with this name is used in other `module`s."
                                    , "Add the generated prism declaration through the fix."
                                    ]
                                , under = "exposing (..)"
                                }
                                |> Review.Test.whenFixed
                                    """module Data.On exposing (some)

import Accessors exposing (Lens, makeOneToN_)
import Data exposing (Data(..))

{-| Accessor prism for the variant `Data.Some` of the `type Data`.


-}
some : Lens (Data a b c d) transformed ( a, ( b, ( c, d ) ) ) wrap
some =
    makeOneToN_
        "Data.Some"
        (\\variantValuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\variantValuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    let
                        ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                            ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter
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


accepted : Test
accepted =
    describe "accepted"
        [ test "variant prism already available"
            (\() ->
                [ """module Data exposing (AlreadyDefined(..))

type AlreadyDefined a
    = DontError a
    | Whatever

variantDontError : Prism (AlreadyDefined a) a
variantDontError = ()
"""
                , """module Data.On exposing (one)

one = one
"""
                , """module Use exposing (use)

import Data.On


use = Data.On.one
"""
                ]
                    |> Review.Test.runOnModules
                        (VariantPrism.GenerateUsed.rule
                            { build = VariantPrism.GenerateUsed.accessors
                            , name = VariantPrism.GenerateUsed.prismNameVariant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectNoErrors
            )
        ]


declarationBuildTestString declarationBuild =
    let
        built =
            declarationBuild
                { variantModule = "Data"
                , typeName = "Data"
                , variantValues = [ CodeGen.typeVar "a", CodeGen.typeVar "b", CodeGen.typeVar "c", CodeGen.typeVar "d" ]
                , typeParameters = [ "a", "b", "c", "d" ]
                , variantName = "Some"
                }
    in
    { name = VariantPrism.GenerateUsed.prismNameVariant.build { variantName = "Some" }
    , documentation = built.documentation
    , annotation = built.annotation
    , implementation = built.implementation
    }
        |> prismDeclarationToCodeGen
        |> declarationToString
