module VariantHelper.GenerateUsed.Test exposing (all)

import Review.Test
import Test exposing (Test, test)
import VariantHelper.GenerateUsed


all : Test
all =
    Test.describe
        "VariantHelper.GenerateUsed"
        [ reported
        , accepted
        , build
        ]


build : Test
build =
    Test.describe
        "build"
        [ buildAccessors
        , buildAccessorsBChiquet
        ]


buildAccessors : Test
buildAccessors =
    Test.describe
        "accessors"
        [ buildAccessorsValuesRecord
        , buildAccessorsValuesTupleNest
        ]


buildAccessorsValuesRecord : Test
buildAccessorsValuesRecord =
    Test.describe
        "values record"
        [ Test.describe
            "variants 1"
            [ test
                "values 1"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.one
"""
                    , """module Data exposing (One(..))

type One
    = One String
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.One` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (one)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (One(..))


{-| Accessor lens for the variant `Data.One` of the `type One`.

-}
one : Lens One transformed String wrap
one =
    makeOneToOne_
        "Data.One"
        (\\(One value0) -> value0)
        (\\valuesAlter (One value0) -> value0 |> valuesAlter |> One)"""
                                ]
                              )
                            ]
                )
            , test
                "values >= 2"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.three
"""
                    , """module Data exposing (Three(..))

type Three a
    = Three String Int a
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.Three` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (three)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Three(..))


{-| Accessor lens for the variant `Data.Three` of the `type Three`.

-}
three : Lens (Three a) transformed { value0 : String, value1 : Int, value2 : a } wrap
three =
    makeOneToOne_
        "Data.Three"
        (\\(Three value0 value1 value2) -> { value0 = value0, value1 = value1, value2 = value2 })
        (\\valuesAlter (Three value0 value1 value2) ->
            let
                altered =
                    { value0 = value0, value1 = value1, value2 = value2 } |> valuesAlter
            in
            Three altered.value0 altered.value1 altered.value2
        )"""
                                ]
                              )
                            ]
                )
            ]
        , Test.describe
            "variants >= 2"
            [ test
                "values 0"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.none
"""
                    , """module Data exposing (Data(..))

type Data
    = Some String
    | None
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.None` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (none)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.None` of the `type Data`.

-}
none : Relation () reachable wrap -> Relation Data reachable (Maybe wrap)
none =
    makeOneToN_
        "Data.None"
        (\\valuesAlter variantType ->
            case variantType of
                None ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\_ -> identity)"""
                                ]
                              )
                            ]
                )
            , test
                "values 1"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.some
"""
                    , """module Data exposing (Data(..))

type Data
    = Some String
    | None
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.Some` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (some)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.Some` of the `type Data`.

-}
some : Relation String reachable wrap -> Relation Data reachable (Maybe wrap)
some =
    makeOneToN_
        "Data.Some"
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 ->
                    value0 |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 ->
                    value0 |> valuesAlter |> Some

                other ->
                    other
        )"""
                                ]
                              )
                            ]
                )
            , test
                "values >= 2"
                (\() ->
                    [ """module Data exposing (Data(..))

type Data a b c d
    = Some a b c d
    | None
"""
                    , """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.some
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.Some` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (some)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.Some` of the `type Data`.

-}
some :
    Relation { value0 : a, value1 : b, value2 : c, value3 : d } reachable wrap
    -> Relation (Data a b c d) reachable (Maybe wrap)
some =
    makeOneToN_
        "Data.Some"
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    { value0 = value0, value1 = value1, value2 = value2, value3 = value3 }
                        |> valuesAlter
                        |> Just

                _ ->
                    Nothing
        )
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 value1 value2 value3 ->
                    let
                        altered =
                            { value0 = value0, value1 = value1, value2 = value2, value3 = value3 }
                                |> valuesAlter
                    in
                    Some altered.value0 altered.value1 altered.value2 altered.value3

                other ->
                    other
        )"""
                                ]
                              )
                            ]
                )
            ]
        ]


buildAccessorsValuesTupleNest : Test
buildAccessorsValuesTupleNest =
    Test.describe
        "valuesTupleNest"
        [ Test.describe
            "variants 1"
            [ test
                "values 1"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.one
"""
                    , """module Data exposing (One(..))

type One
    = One String
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.One` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (one)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (One(..))


{-| Accessor lens for the variant `Data.One` of the `type One`.

-}
one : Lens One transformed String wrap
one =
    makeOneToOne_
        "Data.One"
        (\\(One value0) -> value0)
        (\\valuesAlter (One value0) -> value0 |> valuesAlter |> One)"""
                                ]
                              )
                            ]
                )
            , test
                "values >= 2"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.three
"""
                    , """module Data exposing (Three(..))

type Three a
    = Three String Int a
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.Three` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (three)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Three(..))


{-| Accessor lens for the variant `Data.Three` of the `type Three`.

-}
three : Lens (Three a) transformed ( String, ( Int, a ) ) wrap
three =
    makeOneToOne_
        "Data.Three"
        (\\(Three value0 value1 value2) -> ( value0, ( value1, value2 ) ))
        (\\valuesAlter (Three value0 value1 value2) ->
            let
                ( alteredValue0, ( alteredValue1, alteredValue2 ) ) =
                    ( value0, ( value1, value2 ) ) |> valuesAlter
            in
            Three alteredValue0 alteredValue1 alteredValue2
        )"""
                                ]
                              )
                            ]
                )
            ]
        , Test.describe
            "variants >= 2"
            [ test
                "values 0"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.none
"""
                    , """module Data exposing (Data(..))

type Data
    = Some String
    | None
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.None` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (none)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.None` of the `type Data`.

-}
none : Relation () reachable wrap -> Relation Data reachable (Maybe wrap)
none =
    makeOneToN_
        "Data.None"
        (\\valuesAlter variantType ->
            case variantType of
                None ->
                    () |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\_ -> identity)"""
                                ]
                              )
                            ]
                )
            , test
                "values 1"
                (\() ->
                    [ """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.some
"""
                    , """module Data exposing (Data(..))

type Data
    = Some String
    | None
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.Some` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (some)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.Some` of the `type Data`.

-}
some : Relation String reachable wrap -> Relation Data reachable (Maybe wrap)
some =
    makeOneToN_
        "Data.Some"
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 ->
                    value0 |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 ->
                    value0 |> valuesAlter |> Some

                other ->
                    other
        )"""
                                ]
                              )
                            ]
                )
            , test
                "values >= 2"
                (\() ->
                    [ """module Data exposing (Data(..))

type Data a b c d
    = Some a b c d
    | None
"""
                    , """module Data.On exposing (..)
"""
                    , """module Use exposing (use)

import Data.On


use =
    Data.On.some
"""
                    ]
                        |> Review.Test.runOnModules
                            (VariantHelper.GenerateUsed.rule
                                { build =
                                    VariantHelper.GenerateUsed.accessors
                                        { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                                , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                                , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                                , generationModuleIsVariantModuleDotSuffix = "On"
                                }
                            )
                        |> Review.Test.expectErrorsForModules
                            [ ( "Data.On"
                              , [ Review.Test.error
                                    { message = "variant helper on `Data.Some` missing"
                                    , details =
                                        [ "A variant helper with this name is used in other `module`s."
                                        , "Add the generated helper declaration through the fix."
                                        ]
                                    , under = "exposing (..)"
                                    }
                                    |> Review.Test.whenFixed
                                        """module Data.On exposing (some)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.Some` of the `type Data`.

-}
some :
    Relation ( a, ( b, ( c, d ) ) ) reachable wrap -> Relation (Data a b c d) reachable (Maybe wrap)
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

                other ->
                    other
        )"""
                                ]
                              )
                            ]
                )
            ]
        ]


buildAccessorsBChiquet : Test
buildAccessorsBChiquet =
    test
        "accessorsBChiquet, valuesTupleNest, variants >= 2, values >= 2"
        (\() ->
            [ """module Data exposing (Data(..))

type Data a b c d
    = Some a b c d
    | None
"""
            , """module Data.On exposing (..)
"""
            , """module Use exposing (use)

import Data.On


use =
    Data.On.some
"""
            ]
                |> Review.Test.runOnModules
                    (VariantHelper.GenerateUsed.rule
                        { build =
                            VariantHelper.GenerateUsed.accessorsBChiquet
                                { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                        , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                        , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                        , generationModuleIsVariantModuleDotSuffix = "On"
                        }
                    )
                |> Review.Test.expectErrorsForModules
                    [ ( "Data.On"
                      , [ Review.Test.error
                            { message = "variant helper on `Data.Some` missing"
                            , details =
                                [ "A variant helper with this name is used in other `module`s."
                                , "Add the generated helper declaration through the fix."
                                ]
                            , under = "exposing (..)"
                            }
                            |> Review.Test.whenFixed
                                """module Data.On exposing (some)

import Accessors exposing (Relation, makeOneToN, makeOneToOne)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.Some` of the `type Data`.

-}
some :
    Relation ( a, ( b, ( c, d ) ) ) reachable wrap -> Relation (Data a b c d) reachable (Maybe wrap)
some =
    makeOneToN
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

                other ->
                    other
        )"""
                        ]
                      )
                    ]
        )


reported : Test
reported =
    Test.describe
        "reported"
        [ test
            "generation `module` missing"
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
                        (VariantHelper.GenerateUsed.rule
                            { build =
                                VariantHelper.GenerateUsed.accessors
                                    { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                            , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                            , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Use"
                          , [ Review.Test.error
                                { message = "variant helper generation `module Data.On` missing"
                                , details =
                                    [ "Create an elm file where variant helpers will be generated in."
                                    , "At the time of writing, [`elm-review` isn't able to generate new files](https://github.com/jfmengels/elm-review/issues/125)."
                                    ]
                                , under = "Data.On.one"
                                }
                            ]
                          )
                        ]
            )
        , test
            "generation `module exposing (..)`, `import` generation `module` missing"
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
                        (VariantHelper.GenerateUsed.rule
                            { build =
                                VariantHelper.GenerateUsed.accessors
                                    { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                            , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                            , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Use"
                          , [ Review.Test.error
                                { message = "`import Data.On` missing"
                                , details =
                                    [ "Add the variant helper generation `module` `import` through the supplied fix."
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
                                { message = "variant helper on `Data.Some` missing"
                                , details =
                                    [ "A variant helper with this name is used in other `module`s."
                                    , "Add the generated helper declaration through the fix."
                                    ]
                                , under = "exposing (..)"
                                }
                                |> Review.Test.whenFixed
                                    """module Data.On exposing (some)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.Some` of the `type Data`.

-}
some :
    Relation ( a, ( b, ( c, d ) ) ) reachable wrap -> Relation (Data a b c d) reachable (Maybe wrap)
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

                other ->
                    other
        )"""
                            ]
                          )
                        ]
            )
        , test
            "generation `module` already `exposing` things"
            (\() ->
                [ """module Data.On exposing (none)

none = none
"""
                , """module Use exposing (use)

import Data.On
use = Data.On.some

"""
                , """module Data exposing (Data(..))

type Data
    = Some String
    | None
"""
                ]
                    |> Review.Test.runOnModules
                        (VariantHelper.GenerateUsed.rule
                            { build =
                                VariantHelper.GenerateUsed.accessors
                                    { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                            , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                            , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Data.On"
                          , [ Review.Test.error
                                { message = "variant helper on `Data.Some` missing"
                                , details =
                                    [ "A variant helper with this name is used in other `module`s."
                                    , "Add the generated helper declaration through the fix."
                                    ]
                                , under = "exposing (none)"
                                }
                                |> Review.Test.whenFixed
                                    """module Data.On exposing (some, none)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)
import Data exposing (Data(..))


{-| Accessor prism for the variant `Data.Some` of the `type Data`.

-}
some : Relation String reachable wrap -> Relation Data reachable (Maybe wrap)
some =
    makeOneToN_
        "Data.Some"
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 ->
                    value0 |> valuesAlter |> Just

                _ ->
                    Nothing
        )
        (\\valuesAlter variantType ->
            case variantType of
                Some value0 ->
                    value0 |> valuesAlter |> Some

                other ->
                    other
        )
none = none
"""
                            ]
                          )
                        ]
            )
        , test
            "variant helper missing in internal module"
            (\() ->
                [ """module Data exposing (One)

type One
    = One String

use = onOne
"""
                ]
                    |> Review.Test.runOnModules
                        (VariantHelper.GenerateUsed.rule
                            { build =
                                VariantHelper.GenerateUsed.accessors
                                    { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                            , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                            , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Data"
                          , [ Review.Test.error
                                { message = "variant helper on `One` missing"
                                , details =
                                    [ "Add the generated helper declaration through the fix."
                                    ]
                                , under = "onOne"
                                }
                                |> Review.Test.whenFixed
                                    """module Data exposing (One)

import Accessors exposing (Lens, Relation, makeOneToN_, makeOneToOne_)

type One
    = One String


{-| Accessor lens for the variant `Data.One` of the `type One`.

-}
onOne : Lens One transformed String wrap
onOne =
    makeOneToOne_
        "Data.One"
        (\\(One value0) -> value0)
        (\\valuesAlter (One value0) -> value0 |> valuesAlter |> One)

use = onOne
"""
                            ]
                          )
                        ]
            )
        ]


accepted : Test
accepted =
    Test.describe
        "accepted"
        [ test
            "variant helper already available in external module"
            (\() ->
                [ """module Data exposing (One(..))

type One
    = One String
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
                        (VariantHelper.GenerateUsed.rule
                            { build =
                                VariantHelper.GenerateUsed.accessors
                                    { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                            , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                            , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectNoErrors
            )
        , test
            "variant helper already available in internal module"
            (\() ->
                [ """module Data exposing (One)

type One
    = One String

use = onOne


onOne = oneOne
"""
                ]
                    |> Review.Test.runOnModules
                        (VariantHelper.GenerateUsed.rule
                            { build =
                                VariantHelper.GenerateUsed.accessors
                                    { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
                            , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
                            , nameInModuleExternal = VariantHelper.GenerateUsed.variant
                            , generationModuleIsVariantModuleDotSuffix = "On"
                            }
                        )
                    |> Review.Test.expectNoErrors
            )
        ]
