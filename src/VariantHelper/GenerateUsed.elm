module VariantHelper.GenerateUsed exposing
    ( rule
    , VariantHelperBuild
    , accessors, accessorsBChiquet
    , documented, annotated, importsAdd
    , variantInMultiple, variantOnly
    , ValuesCombined, valuesTupleNest, valuesRecord
    , variantPattern
    , VariantHelperNameConfig, onVariant, variant
    )

{-| Generate helpers for variant values

@docs rule


## build

@docs VariantHelperBuild
@docs accessors, accessorsBChiquet
@docs documented, annotated, importsAdd
@docs variantInMultiple, variantOnly
@docs ValuesCombined, valuesTupleNest, valuesRecord
@docs variantPattern


## name

@docs VariantHelperNameConfig, onVariant, variant

-}

import Dict exposing (Dict)
import Elm.CodeGen as CodeGen
import Elm.Docs as Meta
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Hand exposing (Hand(..))
import Help exposing (beforeSuffixParser, char0ToLower, char0ToUpper, declarationToString, importsToString, indexed, metaToVariantType, onColumn, qualifiedSyntaxToString, typeLens, typeRelation)
import Parser exposing ((|.), (|=), Parser)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import Stack
import VariantHelper.GenerateUsed.Testable exposing (helperDeclarationToCodeGen)


{-| Generate each helper for a variant of a `type`
that is called from your code but isn't already defined.

    import Review.Rule as Rule exposing (Rule)
    import VariantHelper.GenerateUsed

    config : List Rule
    config =
        [ VariantHelper.GenerateUsed.rule ..config..
        ]

..config.. How to generate, where to generate:

  - `build :`
    a [`Build` function](#Build) like
      - [`accessors`](#accessors)
      - [`accessorsBChiquet`](#accessorsBChiquet)
  - `name :`
    a way to handle variant helper names like
      - [`onVariant`](#onVariant)
      - [`variant`](#variant)
  - `generationModuleIsVariantModuleDotSuffix :`
    a `.Suffix` to derive generation `module` names from variant `module` names

There's no configuration to automatically `import Variant.Module.Generation as Variant.Module`
because [`import` aliases can't contain `.`](https://github.com/elm/compiler/issues/2260)


### example `module Variant.Module.On exposing (some)`

    { build =
        VariantHelper.GenerateUsed.accessors
            { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
    , nameInModuleInternal = VariantHelper.GenerateUsed.variant
    , nameInModuleExternal = VariantHelper.GenerateUsed.onVariant
    , generationModuleIsVariantModuleDotSuffix = "On"
    }


### example: `module Variant.Module.X exposing (onSome)`

    { build =
        VariantHelper.GenerateUsed.accessors
            { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
    , nameInModuleInternal = VariantHelper.GenerateUsed.onVariant
    , nameInModuleExternal = VariantHelper.GenerateUsed.onVariant
    , generationModuleIsVariantModuleDotSuffix = "X"
    }

---

Variant helpers will be generated below the `type` declaration.
Consider [`SiriusStarr/elm-review-no-unsorted`](https://dark.elm.dmy.fr/packages/SiriusStarr/elm-review-no-unsorted/latest/NoUnsortedTopLevelDeclarations)
for more fine-grained and consistent positioning control


## use it

... when you're using `elm-accessors` to mitigate
boilerplate related to updating potentially deeply nested data.


## don't use it

... when you consider accessors the less readable/intuitive/simple/explicit alternative.

-}
rule :
    { nameInModuleInternal : VariantHelperNameConfig
    , nameInModuleExternal : VariantHelperNameConfig
    , build : VariantHelperBuild
    , generationModuleIsVariantModuleDotSuffix : String
    }
    -> Rule
rule config =
    ruleImplementation
        { build = config.build
        , nameInModuleInternal =
            config.nameInModuleInternal
                |> variantHelperNameConfigCorrectCasing
        , nameInModuleExternal =
            config.nameInModuleExternal
                |> variantHelperNameConfigCorrectCasing
        , generationModuleSuffix = config.generationModuleIsVariantModuleDotSuffix
        }


variantHelperNameConfigCorrectCasing : VariantHelperNameConfig -> VariantHelperNameConfig
variantHelperNameConfigCorrectCasing =
    \nameConfig ->
        { build = \variantName -> nameConfig.build variantName |> char0ToLower
        , parser =
            nameConfig.parser
                |> Parser.map
                    (\{ variantName } ->
                        { variantName = variantName |> char0ToUpper }
                    )
        }



-- config


type alias Configuration =
    RecordWithoutConstructorFunction
        { build : VariantHelperBuild
        , nameInModuleInternal : VariantHelperNameConfig
        , nameInModuleExternal : VariantHelperNameConfig
        , generationModuleSuffix : String
        }


{-| Helpers for values of a given only variant.

for

    type Id attachment
        = Id (List Int) attachment


#### `access`

    \(Id value0 value1) -> ( value0, value1 )


#### `alter`

    \valuesAlter (Id value0 value1) ->
        let
            ( alteredValue0, alteredValue1 ) =
                ( value0, value1 ) |> valuesAlter
        in
        Id alteredValue0 alteredValue1

-}
variantOnly :
    { name : String
    , values : List CodeGen.TypeAnnotation
    , valuesCombined : ValuesCombined
    }
    ->
        { access : CodeGen.Expression
        , alter : CodeGen.Expression
        , typeValues : CodeGen.TypeAnnotation
        }
variantOnly variantInfo =
    let
        variantPatternBuilt =
            variantPattern
                { name = variantInfo.name
                , values = variantInfo.values
                }

        valuesCombined =
            variantInfo.valuesCombined
                { name = variantInfo.name
                , values = variantInfo.values
                }
    in
    { access =
        CodeGen.lambda
            [ variantPatternBuilt ]
            (valuesCombined.inOne "value")
    , alter =
        CodeGen.lambda
            [ CodeGen.varPattern "valuesAlter"
            , variantPatternBuilt
            ]
            valuesCombined.alter
    , typeValues = valuesCombined.typeInOne
    }


{-| Helpers for values of a given variant among >= 2.

for

    type Data a b c d
        = Some a b c d
        | None

with

    variantInMultiple valuesTupleNest


#### `access`

    \valuesAlter variantType ->
        case variantType of
            Some value0 value1 value2 value3 ->
                ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter |> Just

            _ ->
                Nothing


#### `alter`

    \valuesAlter variantType ->
        case variantType of
            Some value0 value1 value2 value3 ->
                let
                    ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                        ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter
                in
                Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

            other ->
                other

-}
variantInMultiple :
    { name : String
    , values : List CodeGen.TypeAnnotation
    , valuesCombined : ValuesCombined
    }
    ->
        { access : CodeGen.Expression
        , alter : CodeGen.Expression
        , typeValues : CodeGen.TypeAnnotation
        }
variantInMultiple variantInfo =
    let
        variantPatternBuilt =
            variantPattern
                { name = variantInfo.name
                , values = variantInfo.values
                }

        valuesCombined =
            variantInfo.valuesCombined
                { name = variantInfo.name
                , values = variantInfo.values
                }
    in
    { access =
        CodeGen.lambda
            [ CodeGen.varPattern "valuesAlter"
            , CodeGen.varPattern "variantType"
            ]
            (CodeGen.caseExpr (CodeGen.val "variantType")
                [ ( variantPatternBuilt
                  , CodeGen.binOpChain
                        (valuesCombined.inOne "value")
                        CodeGen.piper
                        [ CodeGen.fun "valuesAlter"
                        , CodeGen.fun "Just"
                        ]
                  )
                , ( CodeGen.allPattern
                  , CodeGen.val "Nothing"
                  )
                ]
            )
    , alter =
        case variantInfo.values of
            [] ->
                CodeGen.lambda
                    [ CodeGen.allPattern ]
                    (CodeGen.val "identity")

            _ :: _ ->
                CodeGen.lambda
                    [ CodeGen.varPattern "valuesAlter"
                    , CodeGen.varPattern "variantType"
                    ]
                    (CodeGen.caseExpr (CodeGen.val "variantType")
                        [ ( variantPatternBuilt
                          , valuesCombined.alter
                          )
                        , ( CodeGen.varPattern "other"
                          , CodeGen.val "other"
                          )
                        ]
                    )
    , typeValues = valuesCombined.typeInOne
    }


{-| Representation of values as one whole, for example

  - [`valuesRecord`](#valuesRecord): `{ value0 = ..., value1 = ..., value2 = ... }`
  - [`valuesTupleNest`](#valuesTupleNest): `( ..., ( ..., ... ) )`
  - [`Toop.T3 ... ... ...`](https://dark.elm.dmy.fr/packages/bburdette/toop/latest/)
  - ...

-}
type alias ValuesCombined =
    { name : String
    , values : List CodeGen.TypeAnnotation
    }
    ->
        { typeInOne : CodeGen.TypeAnnotation
        , inOne : String -> CodeGen.Expression
        , patternInOne : String -> CodeGen.Pattern
        , alter : CodeGen.Expression
        }


{-| Helpers for a given variant to use with a custom implementation or [`variantOnly`](#variantOnly)/[`variantInMultiple`](#variantInMultiple).

for

    type Data a b c d
        = Some a b c d
        | None


#### `typeInOne`

    ( a, ( b, ( c, d ) ) )


#### `inOne "value"`

    ( value0, ( value1, ( value2, value3 ) ) )


#### `patternInOne "value"`

    ( value0, ( value1, ( value2, value3 ) ) )


#### `alter`

    let
        ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
            ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter
    in
    Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

-}
valuesTupleNest : ValuesCombined
valuesTupleNest variantInfo =
    let
        { inOne, patternInOne } =
            case variantInfo.values |> Stack.fromList of
                Empty _ ->
                    { inOne = \_ -> CodeGen.unit
                    , patternInOne = \_ -> CodeGen.unitPattern
                    }

                Filled stacked ->
                    { inOne =
                        \valueName ->
                            Filled stacked
                                |> Stack.map
                                    (\{ index } _ ->
                                        CodeGen.val (valueName |> indexed index)
                                    )
                                |> Stack.reverse
                                |> Stack.fold (\value soFar -> CodeGen.tuple [ value, soFar ])
                    , patternInOne =
                        \valueName ->
                            Filled stacked
                                |> Stack.map
                                    (\{ index } _ ->
                                        CodeGen.varPattern (valueName |> indexed index)
                                    )
                                |> Stack.reverse
                                |> Stack.fold (\value soFar -> CodeGen.tuplePattern [ value, soFar ])
                    }
    in
    { typeInOne =
        case variantInfo.values |> Stack.fromList of
            Empty _ ->
                CodeGen.unitAnn

            Filled stackFill ->
                Filled stackFill
                    |> Stack.reverse
                    |> Stack.fold
                        (\value soFar -> CodeGen.tupleAnn [ value, soFar ])
    , patternInOne = patternInOne
    , inOne = inOne
    , alter =
        case variantInfo.values of
            [] ->
                CodeGen.letExpr
                    [ CodeGen.letDestructuring
                        CodeGen.unitPattern
                        (CodeGen.applyBinOp
                            CodeGen.unit
                            CodeGen.piper
                            (CodeGen.fun "valuesAlter")
                        )
                    ]
                    (CodeGen.val variantInfo.name)

            [ _ ] ->
                CodeGen.binOpChain
                    (CodeGen.val ("value" |> indexed 0))
                    CodeGen.piper
                    [ CodeGen.fun "valuesAlter"
                    , CodeGen.fun variantInfo.name
                    ]

            -- >= 2
            element0 :: element1 :: elements2Up ->
                let
                    variantValuesStack =
                        Stack.topDown element0 (element1 :: elements2Up)
                in
                CodeGen.letExpr
                    [ CodeGen.letDestructuring
                        (patternInOne "alteredValue")
                        (CodeGen.applyBinOp
                            (inOne "value")
                            CodeGen.piper
                            (CodeGen.fun "valuesAlter")
                        )
                    ]
                    (CodeGen.construct variantInfo.name
                        (variantValuesStack
                            |> Stack.map
                                (\{ index } _ ->
                                    CodeGen.val ("alteredValue" |> indexed index)
                                )
                            |> Stack.toList
                        )
                    )
    }


{-| Pattern on a given variant to use with a custom implementation.

for

    type Data a b c d
        = Some a b c d
        | None

generates

    Some value0 value1 value2 value3

-}
variantPattern :
    { name : String
    , values : List CodeGen.TypeAnnotation
    }
    -> CodeGen.Pattern
variantPattern { name, values } =
    CodeGen.namedPattern name
        (values
            |> List.indexedMap
                (\index _ ->
                    CodeGen.varPattern ("value" |> indexed index)
                )
        )


{-| Helpers for a given variant to use with a custom implementation or [`variantOnly`](#variantOnly)/[`variantInMultiple`](#variantInMultiple).

for

    type Data a b c d
        = Some a b c d
        | None


#### `access`

    { value0 = value0, value1 = value1, value2 = value2, value3 = value3 }


#### `alter`

    let
        altered =
            { value0 = value0, value1 = value1, value2 = value2, value3 = value3 }
                |> valuesAlter
    in
    Some altered.value0 altered.value1 altered.value2 altered.value3


#### `typeValues`

    { value0 = a, value1 = b, value2 = c, value3 = d }

-}
valuesRecord : ValuesCombined
valuesRecord variantInfo =
    let
        inOne valueName =
            case variantInfo.values of
                [] ->
                    CodeGen.unit

                [ _ ] ->
                    CodeGen.val (valueName |> indexed 0)

                value0 :: value1 :: valuesFrom2 ->
                    (value0 :: value1 :: valuesFrom2)
                        |> List.indexedMap
                            (\index _ ->
                                ( valueName |> indexed index
                                , CodeGen.val (valueName |> indexed index)
                                )
                            )
                        |> CodeGen.record
    in
    { typeInOne =
        case variantInfo.values of
            [] ->
                CodeGen.unitAnn

            [ valueOnly ] ->
                valueOnly

            value0 :: value1 :: value2Up ->
                (value0 :: value1 :: value2Up)
                    |> List.indexedMap
                        (\index value ->
                            ( "value" |> indexed index, value )
                        )
                    |> CodeGen.recordAnn
    , patternInOne =
        case variantInfo.values of
            [] ->
                \_ -> CodeGen.unitPattern

            -- >= 2
            value0 :: values1Up ->
                \valueName ->
                    (value0 :: values1Up)
                        |> List.indexedMap
                            (\index _ -> valueName |> indexed index)
                        |> CodeGen.recordPattern
    , inOne = inOne
    , alter =
        case variantInfo.values of
            [] ->
                CodeGen.val variantInfo.name

            [ _ ] ->
                CodeGen.binOpChain
                    (CodeGen.val ("value" |> indexed 0))
                    CodeGen.piper
                    [ CodeGen.fun "valuesAlter"
                    , CodeGen.fun variantInfo.name
                    ]

            -- >= 2
            value0 :: value1 :: values2Up ->
                let
                    variantValuesList =
                        value0 :: value1 :: values2Up
                in
                CodeGen.letExpr
                    [ CodeGen.letDestructuring
                        (CodeGen.varPattern "altered")
                        (CodeGen.applyBinOp
                            (inOne "value")
                            CodeGen.piper
                            (CodeGen.fun "valuesAlter")
                        )
                    ]
                    (CodeGen.construct variantInfo.name
                        (variantValuesList
                            |> List.indexedMap
                                (\index _ ->
                                    CodeGen.access
                                        (CodeGen.val "altered")
                                        ("value" |> indexed index)
                                )
                        )
                    )
    }


{-| [`Build`](#Build)
of named [`erlandsona/elm-accessors`](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/)
which with

    { build =
        VariantHelper.GenerateUsed.accessors
            { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
    , nameInModuleInternal = VariantHelper.GenerateUsed.onVariant
    , nameInModuleExternal = VariantHelper.GenerateUsed.variant
    , generationModuleIsVariantModuleDotSuffix = "On"
    }

and

    module Data exposing (Data(..))

    type Data a b c d
        = Some a b c d
        | None

generates

    module Data.On exposing (some)

    import Accessors exposing (makeOneToN_)
    import Data exposing (Data(..))

    {-| Accessor prism for the variant `Data.Some` of the `type Data`.
    -}
    some :
        Relation ( a, ( b, ( c, d ) ) ) reachable wrap
        -> Relation (Data a b c d) reachable (Maybe wrap)
    some =
        makeOneToN_
            "Data.Some"
            (\valuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter |> Just

                    _ ->
                        Nothing
            )
            (\valuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        let
                            ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                                ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter
                        in
                        Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

                    other ->
                        other
            )

-}
accessors :
    { valuesCombined : ValuesCombined }
    -> VariantHelperBuild
accessors { valuesCombined } =
    \{ variantName, typeName, variantValues, typeParameters, variantModule, otherVariants } ->
        let
            qualifiedVariantName =
                [ variantModule, ".", variantName ]
                    |> String.concat

            { implementation, typeVariantValues, name } =
                if otherVariants |> Dict.isEmpty then
                    let
                        { access, alter, typeValues } =
                            variantOnly
                                { name = variantName
                                , values = variantValues
                                , valuesCombined = valuesCombined
                                }
                    in
                    { name = "lens"
                    , implementation =
                        CodeGen.construct "makeOneToOne_"
                            [ CodeGen.string qualifiedVariantName
                            , access
                            , alter
                            ]
                    , typeVariantValues = typeValues
                    }

                else
                    let
                        { access, alter, typeValues } =
                            variantInMultiple
                                { name = variantName
                                , values = variantValues
                                , valuesCombined = valuesCombined
                                }
                    in
                    { name = "prism"
                    , implementation =
                        CodeGen.construct "makeOneToN_"
                            [ CodeGen.string qualifiedVariantName
                            , access
                            , alter
                            ]
                    , typeVariantValues = typeValues
                    }
        in
        { imports =
            [ CodeGen.importStmt [ "Accessors" ]
                Nothing
                ([ CodeGen.typeOrAliasExpose "Relation"
                 , CodeGen.typeOrAliasExpose "Lens"
                 , CodeGen.funExpose "makeOneToN_"
                 , CodeGen.funExpose "makeOneToOne_"
                 ]
                    |> CodeGen.exposeExplicit
                    |> Just
                )
            ]
        , documentation =
            CodeGen.emptyDocComment
                |> CodeGen.markdown
                    ([ "Accessor "
                     , name
                     , " for the variant `"
                     , qualifiedVariantName
                     , "` of the `type "
                     , typeName
                     , "`."
                     ]
                        |> String.concat
                    )
                |> Just
        , annotation =
            let
                typeUnion =
                    CodeGen.typed typeName
                        (typeParameters |> List.map CodeGen.typeVar)
            in
            (if otherVariants |> Dict.isEmpty then
                typeLens
                    typeUnion
                    (CodeGen.typeVar "transformed")
                    typeVariantValues
                    (CodeGen.typeVar "wrap")

             else
                CodeGen.funAnn
                    (typeRelation
                        typeVariantValues
                        (CodeGen.typeVar "reachable")
                        (CodeGen.typeVar "wrap")
                    )
                    (typeRelation
                        typeUnion
                        (CodeGen.typeVar "reachable")
                        (CodeGen.maybeAnn (CodeGen.typeVar "wrap"))
                    )
            )
                |> Just
        , implementation = implementation
        }


{-| [`Build`](#Build)
of unnamed [bChiquet/elm-accessors](https://dark.elm.dmy.fr/packages/bChiquet/elm-accessors/latest/)
which with

    { build =
        VariantHelper.GenerateUsed.accessorsBChiquet
            { valuesCombined = VariantHelper.GenerateUsed.valuesTupleNest }
    , nameInModuleInternal = VariantHelper.GenerateUsed.variant
    , nameInModuleExternal = VariantHelper.GenerateUsed.onVariant
    , generationModuleIsVariantModuleDotSuffix = "On"
    }

and

    module Data exposing (Data(..))

    type Data a b c d
        = Some a b c d
        | None

generates

    module Data.On exposing (some)

    import Accessors exposing (Lens, Relation, makeOneToN_)
    import Data exposing (Data(..))

    {-| Accessor prism for the variant `Data.Some` of the `type Data`.
    -}
    some :
        Relation ( a, ( b, ( c, d ) ) ) reachable wrap
        -> Relation (Data a b c d) reachable (Maybe wrap)
    some =
        makeOneToN
            (\valuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter |> Just

                    _ ->
                        Nothing
            )
            (\valuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        let
                            ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                                ( value0, ( value1, ( value2, value3 ) ) ) |> valuesAlter
                        in
                        Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

                    other ->
                        other
            )

-}
accessorsBChiquet :
    { valuesCombined : ValuesCombined }
    -> VariantHelperBuild
accessorsBChiquet { valuesCombined } =
    \{ variantName, typeName, variantValues, typeParameters, variantModule, otherVariants } ->
        let
            { implementation, typeVariantValues, name } =
                if otherVariants |> Dict.isEmpty then
                    let
                        { access, alter, typeValues } =
                            variantOnly
                                { name = variantName
                                , values = variantValues
                                , valuesCombined = valuesCombined
                                }
                    in
                    { name = "lens"
                    , implementation = CodeGen.construct "makeOneToOne" [ access, alter ]
                    , typeVariantValues = typeValues
                    }

                else
                    let
                        { access, alter, typeValues } =
                            variantInMultiple
                                { name = variantName
                                , values = variantValues
                                , valuesCombined = valuesCombined
                                }
                    in
                    { name = "prism"
                    , implementation = CodeGen.construct "makeOneToN" [ access, alter ]
                    , typeVariantValues = typeValues
                    }
        in
        { imports =
            [ CodeGen.importStmt [ "Accessors" ]
                Nothing
                ([ CodeGen.typeOrAliasExpose "Relation"
                 , CodeGen.funExpose "makeOneToN"
                 , CodeGen.funExpose "makeOneToOne"
                 ]
                    |> CodeGen.exposeExplicit
                    |> Just
                )
            ]
        , documentation =
            CodeGen.emptyDocComment
                |> CodeGen.markdown
                    ([ "Accessor "
                     , name
                     , " for the variant `"
                     , variantModule
                     , "."
                     , variantName
                     , "` of the `type "
                     , typeName
                     , "`."
                     ]
                        |> String.concat
                    )
                |> Just
        , annotation =
            CodeGen.funAnn
                (typeRelation
                    typeVariantValues
                    (CodeGen.typeVar "reachable")
                    (CodeGen.typeVar "wrap")
                )
                (typeRelation
                    (CodeGen.typed typeName
                        (typeParameters |> List.map CodeGen.typeVar)
                    )
                    (CodeGen.typeVar "reachable")
                    (if otherVariants |> Dict.isEmpty then
                        CodeGen.typeVar "wrap"

                     else
                        CodeGen.maybeAnn (CodeGen.typeVar "wrap")
                    )
                )
                |> Just
        , implementation = implementation
        }


{-| How to derive helper name ↔ variant name.

Out of the box, there are

  - [`onVariant`](onVariant)
  - [`variant`](#variant)

You can also create a custom [`VariantHelperNameConfig`](#VariantHelperNameConfig):

    import Parser

    { build = \{ variantName } -> variantName ++ "Variant"
    , parser =
        Parser.map (\variantName -> { variantName = variantName })
            (Parser.loop ""
                (\beforeSuffixFoFar ->
                    Parser.oneOf
                        [ Parser.token "Variant"
                            |. Parser.end
                            |> Parser.map (\() -> Parser.Done beforeSuffixFoFar)
                        , Parser.chompIf (\_ -> True)
                            |> Parser.getChompedString
                            |> Parser.map
                                (\stillNotSuffix ->
                                    Parser.Loop (beforeSuffixFoFar ++ stillNotSuffix)
                                )
                        ]
                )
            )
    }

It's not half as daunting as it looks. If you feel motivated 👀 ↓

  - a [video guide "Demystifying Parsers" by Tereza Sokol](https://m.youtube.com/watch?v=M9ulswr1z0E)
  - an ["Introduction to the elm/parser package" by Alex Korban](https://korban.net/posts/elm/2018-09-07-introduction-elm-parser/)
  - [the `elm/parser` package](https://dark.elm.dmy.fr/packages/elm/parser/latest/)

Mini tip: testing is always a good idea for `Parser`s

Don't worry about the casing of the results.
They will be automatically be corrected when passed to [`rule`](#rule).

In the future,
[`elm-morph`](https://github.com/lue-bird/elm-morph)
will allow creating builders and parsers in one go,
making this easier.

-}
type alias VariantHelperNameConfig =
    RecordWithoutConstructorFunction
        { parser : Parser { variantName : String }
        , build : { variantName : String } -> String
        }


{-| Handle helper names in the format `on<Variant>`.
Check out [`VariantHelperNameConfig`](#VariantHelperNameConfig) for all naming options.

    import Parser
    import VariantHelper.GenerateUsed

    "onSuccess"
        |> Parser.run VariantHelper.GenerateUsed.onVariant.parser
    --> { variantName = "Success" }

    { variantName = "Success" }
        |> VariantHelper.GenerateUsed.onVariant.build
    --> "onSuccess"

-}
onVariant : VariantHelperNameConfig
onVariant =
    { build = \{ variantName } -> "on" ++ variantName
    , parser =
        Parser.succeed (\variantName -> { variantName = variantName })
            |. Parser.token "on"
            |= (Parser.chompWhile (\_ -> True)
                    |> Parser.getChompedString
               )
    }


{-| Handle helper names in the format `<variant>`.
Check out [`VariantHelperNameConfig`](#VariantHelperNameConfig) for all naming options.

    import Parser
    import VariantHelper.GenerateUsed

    "success"
        |> Parser.run VariantHelper.GenerateUsed.helperNameAsVariant.parser
    --> { variantName = "Success" }

    { variantName = "Success" }
        |> VariantHelper.GenerateUsed.helperNameAsVariant.build
    --> "success"

-}
variant : VariantHelperNameConfig
variant =
    { build = \{ variantName } -> variantName |> char0ToLower
    , parser =
        Parser.map
            (\variantName ->
                { variantName = variantName |> char0ToUpper }
            )
            (Parser.chompWhile (\_ -> True)
                |> Parser.getChompedString
            )
    }


{-| Configure
how to generate a variant helper declaration
plus the necessary `import`s.

Out of the box, there are

  - [`accessors`](#accessors)
  - [`accessorsBChiquet`](#accessorsBChiquet)

Customize with

  - [`documented`](#documented)
  - [`annotated`](#annotated)
  - [`importsAdd`](#importsAdd)

Create a custom helper generator (or just parts for replacement) with

  - [`the-sett/elm-syntax-dsl`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest)
  - [`variantInMultiple`](#variantInMultiple), [`variantOnly`](#variantOnly)
  - [`valuesTupleNest`](#valuesTupleNest) , [`valuesRecord`](#valuesRecord)
  - [`variantPattern`](#variantPattern)

You can use the source code of [`accessors`](#accessors) & co. as a starting point.

-}
type alias VariantHelperBuild =
    { variantModule : String
    , typeName : String
    , typeParameters : List String
    , variantName : String
    , variantValues : List CodeGen.TypeAnnotation
    , otherVariants : Dict String { valueCount : Int }
    }
    ->
        { imports : List CodeGen.Import
        , documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
        , annotation : Maybe CodeGen.TypeAnnotation
        , implementation : CodeGen.Expression
        }


{-| [Build](#VariantHelperBuild) a different documentation:

    accessorsDocumentedCustom info =
        accessors
            { valuesCombined = valuesRecord }
            info
            |> documented
                (emptyDocComment
                    |> markdown
                        ("variant `" ++ info.variantName ++ "`: Accessor for the values.")
                )

-}
documented :
    CodeGen.Comment CodeGen.DocComment
    ->
        { declaration
            | documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
        }
    ->
        { declaration
            | documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
        }
documented docCommentReplacement =
    \declaration ->
        { declaration
            | documentation = docCommentReplacement |> Just
        }


{-| [Build](#Build) a different type annotation:

    import Hand exposing (Hand(..))
    import Stack

    accessorsAnnotatedOption : Build
    accessorsAnnotatedOption info =
        accessors
            { valuesCombined = valuesRecord }
            info
            |> annotated
                (typed "Option"
                    [ CodeGen.typed info.typeName
                        (info.typeParameters |> List.map CodeGen.typeVar)
                    , case variantValues |> Stack.fromList of
                        Empty _ ->
                            CodeGen.unitAnn

                        Filled stacked ->
                            Filled stacked
                                |> Stack.reverse
                                |> Stack.fold (\value soFar -> CodeGen.tupleAnn [ value, soFar ])
                    , CodeGen.typeVar "reachable"
                    , CodeGen.typeVar "wrap"
                    ]
                )
            |> importsAdd
                [ impostStmt [ "Accessors" ]
                    Nothing
                    ([ "Option" |> typeOrAliasExpose ] |> exposingExplicit |> Just)
                ]

Make sure to [`importsAdd`](#importsAdd).

-}
annotated :
    CodeGen.TypeAnnotation
    ->
        { declaration
            | annotation : Maybe CodeGen.TypeAnnotation
        }
    ->
        { declaration
            | annotation : Maybe CodeGen.TypeAnnotation
        }
annotated annotationReplacement =
    \declaration ->
        { declaration
            | annotation = annotationReplacement |> Just
        }


{-| Supply additional `import`s required for generating the declaration.

    accessorsAnnotatedOption : Build
    accessorsAnnotatedOption info =
        accessors info
            |> annotated (typed "Option" [ ... ])
            |> importsAdd
                [ impostStmt [ "Accessors" ]
                    Nothing
                    ([ "Option" |> typeOrAliasExpose ] |> exposingExplicit |> Just)
                ]

-}
importsAdd :
    List CodeGen.Import
    -> { declaration | imports : List CodeGen.Import }
    -> { declaration | imports : List CodeGen.Import }
importsAdd importsAdditional =
    \declaration ->
        { declaration
            | imports = declaration.imports ++ importsAdditional
        }



-- implementation


ruleImplementation : Configuration -> Rule
ruleImplementation config =
    Rule.newProjectRuleSchema
        "VariantHelper.GenerateUsed"
        initialProjectContext
        |> Rule.withDependenciesProjectVisitor
            (\dependencies context ->
                ( []
                , let
                    modules =
                        dependencies
                            |> Dict.values
                            |> List.concatMap Dependency.modules
                  in
                  context |> dependencyModulesVisit modules
                )
            )
        |> Rule.withModuleVisitor
            (\moduleSchema ->
                moduleSchema
                    |> Rule.withModuleDefinitionVisitor
                        (\(Node _ moduleHeader) context ->
                            ( []
                            , context |> moduleHeaderVisit moduleHeader
                            )
                        )
                    |> Rule.withImportVisitor
                        (\importNode context ->
                            ( []
                            , context |> importVisit importNode
                            )
                        )
                    |> Rule.withExpressionEnterVisitor
                        (\expressionNode context ->
                            ( []
                            , context
                                |> expressionVisit
                                    { helperModuleInternalNameParser = config.nameInModuleInternal.parser
                                    , helperModuleExternalNameParser = config.nameInModuleExternal.parser
                                    , generationModuleSuffix = config.generationModuleSuffix
                                    , expressionNode = expressionNode
                                    }
                            )
                        )
                    |> Rule.withDeclarationEnterVisitor
                        (\declaration context ->
                            ( []
                            , context
                                |> declarationVisit
                                    { helperModuleExternalNameParser = config.nameInModuleExternal.parser
                                    , helperModuleInternalNameParser = config.nameInModuleInternal.parser
                                    , declaration = declaration
                                    }
                            )
                        )
                    |> Rule.withFinalModuleEvaluation (moduleInternalHelpersGenerate config)
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule =
                projectContextToModule
                    { generationModuleSuffix = config.generationModuleSuffix
                    }
            , fromModuleToProject =
                moduleContextToProject
                    { generationModuleSuffix = config.generationModuleSuffix
                    }
            , foldProjectContexts = projectContextsFold
            }
        |> Rule.withFinalProjectEvaluation
            (\context ->
                generateForProject { context = context, config = config }
            )
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    RecordWithoutConstructorFunction
        { variantTypes :
            Dict
                -- by `module` name
                String
                (Dict
                    String
                    { variants :
                        Dict String (List CodeGen.TypeAnnotation)
                    , parameters : List String
                    }
                )
        , useModules :
            Dict
                String
                { key : Rule.ModuleKey
                , belowImportsRow : Int
                , helpersFromModuleExternalUsed :
                    Dict
                        -- by variant origin module without the generation module suffix
                        String
                        { import_ : Presence
                        , variantsOfUses : Dict String Range
                        }
                }
        , generationModules :
            Dict
                -- by variant origin module without the generation module suffix
                String
                { exposing_ : Node Exposing
                , -- the location to insert possible declarations first, then `import`s
                  belowImportsRow : Int
                , available : Set String
                , key : Rule.ModuleKey
                }
        }


type Presence
    = Present
    | Missing


{-| `generationModule` is `Just` if the reviewed `module` is one where variant helpers are generated in
-}
type alias ModuleContext =
    RecordWithoutConstructorFunction
        { name : String
        , moduleOriginLookup : ModuleNameLookupTable
        , helpersFromModuleExternalUsed :
            Dict
                -- by variant origin module without the generation module suffix
                String
                (Dict String Range)
        , helpersFromModuleInternalUsed :
            Dict
                -- by variant name
                String
                (List Range)
        , variantTypes :
            Dict
                String
                { variants : Dict String (List CodeGen.TypeAnnotation)
                , parameters : List String
                , belowDeclarationRow : Int
                }
        , -- the location to insert possible declarations first, then `import`s
          belowImportsRow : Int
        , importedModules : Set String
        , helpersModuleInternalAvailable :
            Set
                -- variant name
                String
        , helpersModuleExternalAvailable :
            Set
                -- variant name
                String
        , generationModule :
            Maybe GenerationModuleContext
        }


type alias GenerationModuleContext =
    RecordWithoutConstructorFunction
        { variantModule : String
        , exposing_ : Node Exposing
        }


initialProjectContext : ProjectContext
initialProjectContext =
    { variantTypes = Dict.empty
    , useModules = Dict.empty
    , generationModules = Dict.empty
    }


projectContextToModule :
    { generationModuleSuffix : String }
    -> Rule.ContextCreator ProjectContext ModuleContext
projectContextToModule { generationModuleSuffix } =
    Rule.initContextCreator
        (\moduleName moduleOriginLookup _ ->
            let
                name =
                    moduleName |> qualifiedSyntaxToString
            in
            initModuleContext
                { name = name
                , moduleOriginLookup = moduleOriginLookup
                , generationModule =
                    case name |> Parser.run (beforeSuffixParser ("." ++ generationModuleSuffix)) of
                        Ok variantModule ->
                            initGenerationModuleContext { variantModule = variantModule }
                                |> Just

                        Err _ ->
                            Nothing
                }
        )
        |> Rule.withModuleName
        |> Rule.withModuleNameLookupTable


initGenerationModuleContext : { variantModule : String } -> GenerationModuleContext
initGenerationModuleContext { variantModule } =
    { variantModule = variantModule
    , exposing_ =
        -- dummy. elm-review doesn't allow context type change after visit
        Exposing.All Range.emptyRange |> Node Range.emptyRange
    }


initModuleContext :
    { name : String
    , moduleOriginLookup : ModuleNameLookupTable
    , generationModule : Maybe GenerationModuleContext
    }
    -> ModuleContext
initModuleContext { moduleOriginLookup, generationModule, name } =
    { name = name
    , moduleOriginLookup = moduleOriginLookup
    , variantTypes = Dict.empty
    , helpersFromModuleExternalUsed = Dict.empty
    , helpersFromModuleInternalUsed = Dict.empty
    , belowImportsRow = 2
    , importedModules = Set.empty
    , helpersModuleInternalAvailable = Set.empty
    , helpersModuleExternalAvailable = Set.empty
    , generationModule = generationModule
    }


moduleContextToProject :
    { generationModuleSuffix : String }
    -> Rule.ContextCreator ModuleContext ProjectContext
moduleContextToProject { generationModuleSuffix } =
    Rule.initContextCreator
        (\meta moduleKey moduleContext ->
            let
                moduleName =
                    meta |> Rule.moduleNameFromMetadata |> qualifiedSyntaxToString
            in
            { variantTypes =
                Dict.singleton
                    moduleName
                    (moduleContext.variantTypes
                        |> Dict.map
                            (\_ typeInfo ->
                                { parameters = typeInfo.parameters
                                , variants = typeInfo.variants
                                }
                            )
                    )
            , generationModules =
                case moduleContext.generationModule of
                    Nothing ->
                        Dict.empty

                    Just generationModuleContext ->
                        Dict.singleton
                            generationModuleContext.variantModule
                            { key = moduleKey
                            , exposing_ = generationModuleContext.exposing_
                            , belowImportsRow = moduleContext.belowImportsRow
                            , available = moduleContext.helpersModuleExternalAvailable
                            }
            , useModules =
                Dict.singleton
                    moduleName
                    { key = moduleKey
                    , belowImportsRow = moduleContext.belowImportsRow
                    , helpersFromModuleExternalUsed =
                        moduleContext.helpersFromModuleExternalUsed
                            |> Dict.map
                                (\variantModule variantsOfUses ->
                                    let
                                        generationModule =
                                            variantModule ++ "." ++ generationModuleSuffix
                                    in
                                    { variantsOfUses = variantsOfUses
                                    , import_ =
                                        if Set.member generationModule moduleContext.importedModules then
                                            Present

                                        else
                                            Missing
                                    }
                                )
                    }
            }
        )
        |> Rule.withMetadata
        |> Rule.withModuleKey


projectContextsFold : ProjectContext -> ProjectContext -> ProjectContext
projectContextsFold context0 context1 =
    { variantTypes =
        context0.variantTypes
            |> Dict.union context1.variantTypes
    , useModules =
        context0.useModules
            |> Dict.union context1.useModules
    , generationModules =
        context0.generationModules
            |> Dict.union context1.generationModules
    }


moduleInternalHelpersGenerate : Configuration -> ModuleContext -> List (Rule.Error {})
moduleInternalHelpersGenerate config =
    \context ->
        let
            helpersToGenerate =
                context.helpersModuleInternalAvailable
                    |> Set.foldl
                        (\available -> Dict.remove available)
                        context.helpersFromModuleInternalUsed
        in
        context.variantTypes
            |> Dict.toList
            |> List.concatMap
                (\( typeName, variantType ) ->
                    let
                        variantValueCounts =
                            variantType.variants
                                |> Dict.map (\_ values -> { valueCount = values |> List.length })
                    in
                    variantType.variants
                        |> Dict.toList
                        |> List.filterMap
                            (\( variantName, variantValues ) ->
                                helpersToGenerate
                                    |> Dict.get variantName
                                    |> Maybe.map
                                        (\helperModuleInternalUseRanges ->
                                            { variantName = variantName
                                            , variantValues = variantValues
                                            , helperModuleInternalUseRanges = helperModuleInternalUseRanges
                                            }
                                        )
                            )
                        |> List.concatMap
                            (\{ variantName, variantValues, helperModuleInternalUseRanges } ->
                                helperModuleInternalUseRanges
                                    |> List.map
                                        (\helperModuleInternalOneUseRange ->
                                            { variantName = variantName
                                            , variantValues = variantValues
                                            , typeName = typeName
                                            , typeParameters = variantType.parameters
                                            , belowTypeDeclarationRow = variantType.belowDeclarationRow
                                            , otherVariants =
                                                variantValueCounts
                                                    |> Dict.remove variantName
                                            , helperModuleInternalOneUseRange = helperModuleInternalOneUseRange
                                            , moduleContext = context
                                            , config = config
                                            }
                                                |> moduleInternalHelperGenerate
                                        )
                            )
                )


moduleInternalHelperGenerate :
    { typeName : String
    , typeParameters : List String
    , belowTypeDeclarationRow : Int
    , variantValues : List CodeGen.TypeAnnotation
    , variantName : String
    , otherVariants : Dict String { valueCount : Int }
    , helperModuleInternalOneUseRange : Range
    , moduleContext : ModuleContext
    , config : Configuration
    }
    -> Rule.Error {}
moduleInternalHelperGenerate { typeParameters, variantValues, variantName, config, typeName, otherVariants, helperModuleInternalOneUseRange, moduleContext, belowTypeDeclarationRow } =
    let
        built =
            config.build
                { variantModule = moduleContext.name
                , typeName = typeName
                , typeParameters = typeParameters
                , variantName = variantName
                , variantValues = variantValues
                , otherVariants = otherVariants
                }

        builtName =
            config.nameInModuleInternal.build { variantName = variantName }
    in
    Rule.errorWithFix
        { message =
            [ "variant helper on `", variantName, "` missing" ]
                |> String.concat
        , details =
            [ "Add the generated helper declaration through the fix."
            ]
        }
        helperModuleInternalOneUseRange
        [ Fix.insertAt
            (belowTypeDeclarationRow |> onColumn 1)
            ([ "\n\n"
             , { name = builtName
               , documentation = built.documentation
               , annotation = built.annotation
               , implementation = built.implementation
               }
                |> helperDeclarationToCodeGen
                |> declarationToString
             , "\n"
             ]
                |> String.concat
            )
        , Fix.insertAt
            (moduleContext.belowImportsRow |> onColumn 1)
            ([ "\n"
             , built.imports
                |> importsToString
             , "\n"
             ]
                |> String.concat
            )
        ]


moduleHeaderVisit : Module -> ModuleContext -> ModuleContext
moduleHeaderVisit moduleHeader =
    \context ->
        case context.generationModule of
            Nothing ->
                context

            Just generationModule ->
                case moduleHeader of
                    Module.NormalModule { exposingList } ->
                        { context
                            | generationModule =
                                { generationModule | exposing_ = exposingList }
                                    |> Just
                        }

                    Module.PortModule _ ->
                        context

                    Module.EffectModule _ ->
                        context


importVisit : Node Import -> ModuleContext -> ModuleContext
importVisit importNode =
    \context ->
        let
            belowImportsRowUpdated =
                { context
                    | belowImportsRow =
                        max context.belowImportsRow
                            ((importNode |> Node.range |> .end |> .row) + 1)
                }
        in
        { belowImportsRowUpdated
            | importedModules =
                belowImportsRowUpdated.importedModules
                    |> Set.insert
                        (importNode
                            |> Node.value
                            |> .moduleName
                            |> Node.value
                            |> qualifiedSyntaxToString
                        )
        }


expressionVisit :
    { helperModuleExternalNameParser : Parser { variantName : String }
    , helperModuleInternalNameParser : Parser { variantName : String }
    , generationModuleSuffix : String
    , expressionNode : Node Expression
    }
    -> ModuleContext
    -> ModuleContext
expressionVisit { helperModuleExternalNameParser, helperModuleInternalNameParser, expressionNode, generationModuleSuffix } =
    case expressionNode |> Node.value of
        Expression.FunctionOrValue qualificationSyntax name ->
            \context ->
                let
                    functionOrValueRange =
                        expressionNode |> Node.range
                in
                case qualificationSyntax of
                    [] ->
                        case name |> Parser.run helperModuleInternalNameParser of
                            Err _ ->
                                context

                            Ok { variantName } ->
                                { context
                                    | helpersFromModuleInternalUsed =
                                        context.helpersFromModuleInternalUsed
                                            |> Dict.update variantName
                                                (Maybe.withDefault []
                                                    >> (::) (expressionNode |> Node.range)
                                                    >> Just
                                                )
                                }

                    _ :: _ ->
                        let
                            generationModule =
                                ModuleNameLookupTable.moduleNameAt
                                    context.moduleOriginLookup
                                    functionOrValueRange
                                    |> -- not imported
                                       Maybe.withDefault qualificationSyntax
                                    |> qualifiedSyntaxToString
                        in
                        case generationModule |> Parser.run (beforeSuffixParser ("." ++ generationModuleSuffix)) of
                            Err _ ->
                                context

                            Ok variantModule ->
                                case name |> Parser.run helperModuleExternalNameParser of
                                    Err _ ->
                                        context

                                    Ok { variantName } ->
                                        { context
                                            | helpersFromModuleExternalUsed =
                                                context.helpersFromModuleExternalUsed
                                                    |> Dict.update
                                                        variantModule
                                                        (\usesSoFar ->
                                                            usesSoFar
                                                                |> Maybe.withDefault Dict.empty
                                                                |> Dict.insert variantName functionOrValueRange
                                                                |> Just
                                                        )
                                        }

        _ ->
            identity


declarationVisit :
    { helperModuleExternalNameParser : Parser { variantName : String }
    , helperModuleInternalNameParser : Parser { variantName : String }
    , declaration : Node Declaration
    }
    -> ModuleContext
    -> ModuleContext
declarationVisit { helperModuleExternalNameParser, helperModuleInternalNameParser, declaration } =
    case declaration |> Node.value of
        Declaration.CustomTypeDeclaration type_ ->
            \moduleContext ->
                { moduleContext
                    | variantTypes =
                        moduleContext.variantTypes
                            |> Dict.insert (type_.name |> Node.value)
                                { parameters =
                                    type_.generics |> List.map Node.value
                                , variants =
                                    type_.constructors
                                        |> List.map
                                            (\(Node _ variantInfo) ->
                                                ( variantInfo.name |> Node.value
                                                , variantInfo.arguments |> List.map Node.value
                                                )
                                            )
                                        |> Dict.fromList
                                , belowDeclarationRow =
                                    (declaration |> Node.range |> .end |> .row) + 1
                                }
                }

        Declaration.FunctionDeclaration functionDeclaration ->
            let
                name =
                    functionDeclaration.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            \moduleContext ->
                { moduleContext
                    | helpersModuleInternalAvailable =
                        moduleContext.helpersModuleInternalAvailable
                            |> (case name |> Parser.run helperModuleInternalNameParser of
                                    Err _ ->
                                        identity

                                    Ok { variantName } ->
                                        Set.insert variantName
                               )
                    , helpersModuleExternalAvailable =
                        moduleContext.helpersModuleInternalAvailable
                            |> (case name |> Parser.run helperModuleExternalNameParser of
                                    Err _ ->
                                        identity

                                    Ok { variantName } ->
                                        Set.insert variantName
                               )
                }

        _ ->
            identity


dependencyModulesVisit : List Meta.Module -> ProjectContext -> ProjectContext
dependencyModulesVisit modules =
    \context ->
        { context
            | variantTypes =
                context.variantTypes
                    |> Dict.union
                        (modules
                            |> List.map
                                (\module_ ->
                                    ( module_.name
                                    , module_.unions
                                        |> List.map metaToVariantType
                                        |> Dict.fromList
                                    )
                                )
                            |> Dict.fromList
                        )
        }


generateForProject :
    { config : Configuration
    , context : ProjectContext
    }
    -> List (Rule.Error { useErrorForModule : () })
generateForProject { context, config } =
    context.useModules
        |> Dict.values
        |> List.concatMap
            (\useModule ->
                useModule.helpersFromModuleExternalUsed
                    |> Dict.toList
                    |> List.concatMap
                        (\( usedVariantOriginModuleName, usedVariantOriginModule ) ->
                            case context.variantTypes |> Dict.get usedVariantOriginModuleName of
                                Nothing ->
                                    []

                                Just variantTypesInModule ->
                                    generateForModule
                                        { maybeGenerationModule =
                                            context.generationModules
                                                |> Dict.get usedVariantOriginModuleName
                                        , usedVariantOriginModule = usedVariantOriginModule
                                        , variantModuleName = usedVariantOriginModuleName
                                        , config = config
                                        , useModuleKey = useModule.key
                                        , useModuleBelowImportsRow = useModule.belowImportsRow
                                        , variantTypes = variantTypesInModule
                                        }
                        )
            )


generateForModule :
    { usedVariantOriginModule :
        { variantsOfUses : Dict String Range, import_ : Presence }
    , useModuleKey : Rule.ModuleKey
    , useModuleBelowImportsRow : Int
    , variantModuleName : String
    , maybeGenerationModule :
        Maybe
            { exposing_ : Node Exposing
            , -- the location to insert possible declarations first, then `import`s
              belowImportsRow : Int
            , available : Set String
            , key : Rule.ModuleKey
            }
    , variantTypes :
        Dict
            String
            { variants : Dict String (List CodeGen.TypeAnnotation)
            , parameters : List String
            }
    , config : Configuration
    }
    -> List (Rule.Error errorScope_)
generateForModule { usedVariantOriginModule, variantModuleName, maybeGenerationModule, useModuleBelowImportsRow, config, variantTypes, useModuleKey } =
    let
        firstUseRange =
            usedVariantOriginModule.variantsOfUses
                |> Dict.values
                |> List.head
                -- not expected
                |> Maybe.withDefault Range.emptyRange

        generationModuleName =
            [ variantModuleName, ".", config.generationModuleSuffix ]
                |> String.concat
    in
    case maybeGenerationModule of
        Nothing ->
            [ Rule.errorForModule useModuleKey
                { message =
                    [ "variant helper generation `module ", generationModuleName, "` missing" ]
                        |> String.concat
                , details =
                    [ "Create an elm file where variant helpers will be generated in."
                    , "At the time of writing, [`elm-review` isn't able to generate new files](https://github.com/jfmengels/elm-review/issues/125)."
                    ]
                }
                firstUseRange
            ]

        Just generationModule ->
            [ case usedVariantOriginModule.import_ of
                Present ->
                    []

                Missing ->
                    [ let
                        importString =
                            [ CodeGen.importStmt [ generationModuleName ] Nothing Nothing ]
                                |> importsToString
                      in
                      Rule.errorForModuleWithFix
                        useModuleKey
                        { message = [ "`", importString, "` missing" ] |> String.concat
                        , details =
                            [ "Add the variant helper generation `module` `import` through the supplied fix."
                            ]
                        }
                        firstUseRange
                        [ [ "\n", importString ]
                            |> String.concat
                            |> Fix.insertAt (useModuleBelowImportsRow |> onColumn 1)
                        ]
                    ]
            , variantTypes
                |> Dict.toList
                |> List.concatMap
                    (\( variantTypeName, variantType ) ->
                        let
                            variantValueCounts =
                                variantType.variants
                                    |> Dict.map (\_ values -> { valueCount = values |> List.length })
                        in
                        variantType.variants
                            |> Dict.filter
                                (\variantName _ ->
                                    Dict.member variantName usedVariantOriginModule.variantsOfUses
                                )
                            |> Dict.filter
                                (\variantName _ ->
                                    Set.member variantName generationModule.available |> not
                                )
                            |> Dict.toList
                            |> List.map
                                (\( variantName, variantValues ) ->
                                    let
                                        built =
                                            config.build
                                                { variantModule = variantModuleName
                                                , typeName = variantTypeName
                                                , typeParameters = variantType.parameters
                                                , variantName = variantName
                                                , variantValues = variantValues
                                                , otherVariants =
                                                    variantValueCounts
                                                        |> Dict.remove variantName
                                                }

                                        builtName =
                                            config.nameInModuleExternal.build { variantName = variantName }
                                    in
                                    Rule.errorForModuleWithFix
                                        generationModule.key
                                        { message =
                                            [ "variant helper on `", variantModuleName, ".", variantName, "` missing" ]
                                                |> String.concat
                                        , details =
                                            [ "A variant helper with this name is used in other `module`s."
                                            , "Add the generated helper declaration through the fix."
                                            ]
                                        }
                                        (generationModule.exposing_ |> Node.range)
                                        [ Fix.insertAt
                                            (generationModule.belowImportsRow |> onColumn 1)
                                            ([ "\n\n"
                                             , { name = builtName
                                               , documentation = built.documentation
                                               , annotation = built.annotation
                                               , implementation = built.implementation
                                               }
                                                |> helperDeclarationToCodeGen
                                                |> declarationToString
                                             ]
                                                |> String.concat
                                            )
                                        , case generationModule.exposing_ of
                                            Node _ (Exposing.All dotDotRange) ->
                                                Fix.replaceRangeBy dotDotRange builtName

                                            Node exposeRange (Exposing.Explicit exposes) ->
                                                (builtName ++ ", ")
                                                    |> Fix.insertAt
                                                        (case exposes of
                                                            (Node firstExposeRange _) :: _ ->
                                                                firstExposeRange.start

                                                            -- exposing () shouldn't parse
                                                            [] ->
                                                                exposeRange.end
                                                        )
                                        , Fix.insertAt
                                            (generationModule.belowImportsRow |> onColumn 1)
                                            ([ "\n"
                                             , [ built.imports
                                               , [ CodeGen.importStmt
                                                    [ variantModuleName ]
                                                    Nothing
                                                    ([ variantTypeName |> CodeGen.openTypeExpose ]
                                                        |> CodeGen.exposeExplicit
                                                        |> Just
                                                    )
                                                 ]
                                               ]
                                                |> List.concat
                                                |> importsToString
                                             , "\n"
                                             ]
                                                |> String.concat
                                            )
                                        ]
                                )
                    )
            ]
                |> List.concat
