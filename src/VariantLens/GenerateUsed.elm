module VariantLens.GenerateUsed exposing
    ( rule
    , VariantLensBuild
    , accessors, accessorsBChiquet
    , documented, annotated, importsAdd
    , variantAmongMultiple, variantOnly
    , ValuesRepresent, tupleNest, valuesRecord
    , variantPattern
    , VariantLensNameConfig, prismNameVariant, prismNameOnVariant
    )

{-| Generate lenses for variant values

@docs rule


## build

@docs VariantLensBuild
@docs accessors, accessorsBChiquet
@docs documented, annotated, importsAdd
@docs variantAmongMultiple, variantOnly
@docs ValuesRepresent, tupleNest, valuesRecord
@docs variantPattern


## name

@docs VariantLensNameConfig, prismNameVariant, prismNameOnVariant

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
import Help exposing (beforeSuffixParser, char0ToLower, char0ToUpper, declarationToString, declarationToVariantType, importsToString, indexed, metaToVariantType, onColumn, qualifiedSyntaxToString, typeLens, typeRelation)
import Parser exposing ((|.), (|=), Parser)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import Stack
import VariantLens.GenerateUsed.Testable exposing (prismDeclarationToCodeGen)


{-| Generate prisms for variant `type`s
that are called from your code but aren't already defined in a dedicated `module`.

    import Review.Rule as Rule exposing (Rule)
    import VariantLens.GenerateUsed

    config : List Rule
    config =
        [ VariantLens.GenerateUsed.rule ..config..
        ]

..config.. How to generate, where to generate:

  - `build :`
    a [`VariantLensBuild` function](#VariantLensBuild) like
      - [`accessors`](#accessors)
      - [`accessorsBChiquet`](#accessorsBChiquet)
  - `name :`
    a way to handle variant lens names like
      - [`prismNameOnVariant`](#prismNameOnVariant)
      - [`prismNameVariant`](#prismNameVariant)
  - `generationModuleIsVariantModuleDotSuffix :`
    a `.Suffix` to derive generation `module` names from variant `module` names

There's no configuration to automatically `import Variant.Module.Generation as Variant.Module`
because [`import` aliases can't contain `.`](https://github.com/elm/compiler/issues/2260)


### example `module Variant.Module.On exposing (some)`

    { build =
        VariantLens.GenerateUsed.accessors
            { valuesRepresent = VariantLens.GenerateUsed.valuesRecord }
    , name = VariantLens.GenerateUsed.prismNameVariant
    , generationModuleIsVariantModuleDotSuffix = "On"
    }


### example: `module Variant.Module.X exposing (onSome)`

    { build =
        VariantLens.GenerateUsed.accessors
            { valuesRepresent = VariantLens.GenerateUsed.valuesRecord }
    , name = VariantLens.GenerateUsed.prismNameOnVariant
    , generationModuleIsVariantModuleDotSuffix = "X"
    }


## use it

... when you're using `elm-accessors` to mitigate
boilerplate related to updating potentially deeply nested data.


## don't use it

... when you consider lenses the less readable/intuitive/simple/explicit alternative.

-}
rule :
    { name : VariantLensNameConfig
    , build : VariantLensBuild
    , generationModuleIsVariantModuleDotSuffix : String
    }
    -> Rule
rule config =
    ruleImplementation
        { build = config.build
        , name =
            { build = \variantName -> config.name.build variantName |> char0ToLower
            , parser =
                config.name.parser
                    |> Parser.map
                        (\{ variantName } ->
                            { variantName = variantName |> char0ToUpper }
                        )
            }
        , generationModuleSuffix = config.generationModuleIsVariantModuleDotSuffix
        }



-- config


type alias Config =
    RecordWithoutConstructorFunction
        { build : VariantLensBuild
        , name : VariantLensNameConfig
        , generationModuleSuffix : String
        }


{-| Helpers for values of a given only variant.

for

    type Id attachment
        = Id (List Int) attachment


#### `access`

    \(Id value0 value1) -> ( value0, value1 )


#### `alter`

    \variantValuesAlter (Id value0 value1) ->
        let
            ( alteredValue0, alteredValue1 ) =
                ( value0, value1 ) |> variantValuesAlter
        in
        Id alteredValue0 alteredValue1

-}
variantOnly :
    { name : String
    , values : List CodeGen.TypeAnnotation
    , valuesRepresent : ValuesRepresent
    }
    ->
        { access : CodeGen.Expression
        , alter : CodeGen.Expression
        , typeValues : CodeGen.TypeAnnotation
        }
variantOnly variant =
    let
        variantPatternBuilt =
            variantPattern
                { name = variant.name
                , values = variant.values
                }

        variantValuesRepresentation =
            variant.valuesRepresent
                { name = variant.name
                , values = variant.values
                }
    in
    { access =
        CodeGen.lambda
            [ variantPatternBuilt ]
            (variantValuesRepresentation.inOne "value")
    , alter =
        CodeGen.lambda
            [ CodeGen.varPattern "variantValuesAlter"
            , variantPatternBuilt
            ]
            variantValuesRepresentation.alter
    , typeValues = variantValuesRepresentation.typeInOne
    }


{-| Helpers for values of a given variant among >= 2.

for

    type Data a b c d
        = Some a b c d
        | None

with

    variantAmongMultiple tupleNest


#### `access`

    \variantValuesAlter variantType ->
        case variantType of
            Some value0 value1 value2 value3 ->
                ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter |> Just

            _ ->
                Nothing


#### `alter`

    \variantValuesAlter variantType ->
        case variantType of
            Some value0 value1 value2 value3 ->
                let
                    ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                        ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter
                in
                Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

            other ->
                other

-}
variantAmongMultiple :
    { name : String
    , values : List CodeGen.TypeAnnotation
    , valuesRepresent : ValuesRepresent
    }
    ->
        { access : CodeGen.Expression
        , alter : CodeGen.Expression
        , typeValues : CodeGen.TypeAnnotation
        }
variantAmongMultiple variant =
    let
        variantPatternBuilt =
            variantPattern
                { name = variant.name
                , values = variant.values
                }

        variantValuesRepresentation =
            variant.valuesRepresent
                { name = variant.name
                , values = variant.values
                }
    in
    { access =
        CodeGen.lambda
            [ CodeGen.varPattern "variantValuesAlter"
            , CodeGen.varPattern "variantType"
            ]
            (CodeGen.caseExpr (CodeGen.val "variantType")
                [ ( variantPatternBuilt
                  , CodeGen.binOpChain
                        (variantValuesRepresentation.inOne "value")
                        CodeGen.piper
                        [ CodeGen.fun "variantValuesAlter"
                        , CodeGen.fun "Just"
                        ]
                  )
                , ( CodeGen.allPattern
                  , CodeGen.val "Nothing"
                  )
                ]
            )
    , alter =
        CodeGen.lambda
            [ CodeGen.varPattern "variantValuesAlter"
            , CodeGen.varPattern "variantType"
            ]
            (CodeGen.caseExpr (CodeGen.val "variantType")
                [ ( variantPatternBuilt
                  , variantValuesRepresentation.alter
                  )
                , ( CodeGen.varPattern "other"
                  , CodeGen.val "other"
                  )
                ]
            )
    , typeValues = variantValuesRepresentation.typeInOne
    }


{-| Representation of values as one whole, for example

  - [`valuesRecord`](#valuesRecord): `{ value0 = ..., value1 = ..., value2 = ... }`
  - [`tupleNest`](#tupleNest): `( ..., ( ..., ... ) )`
  - [`Toop.T3 ... ... ...`](https://dark.elm.dmy.fr/packages/bburdette/toop/latest/)
  - ...

-}
type alias ValuesRepresent =
    { name : String
    , values : List CodeGen.TypeAnnotation
    }
    ->
        { typeInOne : CodeGen.TypeAnnotation
        , inOne : String -> CodeGen.Expression
        , patternInOne : String -> CodeGen.Pattern
        , alter : CodeGen.Expression
        }


{-| Helpers for a given variant to use with a custom implementation or [`variantOnly`](#variantOnly)/[`variantAmongMultiple`](#variantAmongMultiple).

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
            ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter
    in
    Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

-}
tupleNest : ValuesRepresent
tupleNest variantInfo =
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
                            (CodeGen.fun "variantValuesAlter")
                        )
                    ]
                    (CodeGen.val variantInfo.name)

            [ _ ] ->
                CodeGen.binOpChain
                    (CodeGen.val ("value" |> indexed 0))
                    CodeGen.piper
                    [ CodeGen.fun "variantValuesAlter"
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
                            (CodeGen.fun "variantValuesAlter")
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


{-| Helpers for a given variant to use with a custom implementation or [`variantOnly`](#variantOnly)/[`variantAmongMultiple`](#variantAmongMultiple).

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
                |> variantValuesAlter
    in
    Some altered.value0 altered.value1 altered.value2 altered.value3


#### `typeValues`

    { value0 = a, value1 = b, value2 = c, value3 = d }

-}
valuesRecord :
    { variantValues : List CodeGen.TypeAnnotation
    , variantName : String
    }
    ->
        { value : CodeGen.Expression
        , alter : CodeGen.Expression
        , typeValues : CodeGen.TypeAnnotation
        }
valuesRecord { variantValues, variantName } =
    { value =
        case variantValues of
            [] ->
                CodeGen.unit

            [ _ ] ->
                CodeGen.val ("value" |> indexed 0)

            value0 :: value1 :: valuesFrom2 ->
                (value0 :: value1 :: valuesFrom2)
                    |> List.indexedMap
                        (\index _ ->
                            ( "value" |> indexed index
                            , CodeGen.val ("value" |> indexed index)
                            )
                        )
                    |> CodeGen.record
    , alter =
        case variantValues of
            [] ->
                CodeGen.letExpr
                    [ CodeGen.letDestructuring
                        CodeGen.unitPattern
                        (CodeGen.applyBinOp
                            CodeGen.unit
                            CodeGen.piper
                            (CodeGen.fun "variantValuesAlter")
                        )
                    ]
                    (CodeGen.val variantName)

            [ _ ] ->
                CodeGen.binOpChain
                    (CodeGen.val ("value" |> indexed 0))
                    CodeGen.piper
                    [ CodeGen.fun "variantValuesAlter"
                    , CodeGen.fun variantName
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
                            (variantValuesList
                                |> List.indexedMap
                                    (\index _ ->
                                        ( "value" |> indexed index
                                        , CodeGen.val ("value" |> indexed index)
                                        )
                                    )
                                |> CodeGen.record
                            )
                            CodeGen.piper
                            (CodeGen.fun "variantValuesAlter")
                        )
                    ]
                    (CodeGen.construct variantName
                        (variantValuesList
                            |> List.indexedMap
                                (\index _ ->
                                    CodeGen.access
                                        (CodeGen.val "altered")
                                        ("value" |> indexed index)
                                )
                        )
                    )
    , typeValues =
        case variantValues of
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
    }


{-| [`VariantLensBuild`](#VariantLensBuild)
of named [`erlandsona/elm-accessors`](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/)
which with

    { build =
        VariantLens.GenerateUsed.accessors
            { valuesRepresent = VariantLens.GenerateUsed.tupleNest }
    , name = VariantLens.GenerateUsed.prismNameVariant
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

    {-| Accessor lens for the variant `Data.Some` of the `type Data`.
    -}
    some :
        Relation ( a, ( b, ( c, d ) ) ) reachable wrap
        -> Relation (Data a b c d) reachable (Maybe wrap)
    some =
        makeOneToN_
            "Data.Some"
            (\variantValuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter |> Just

                    _ ->
                        Nothing
            )
            (\variantValuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        let
                            ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                                ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter
                        in
                        Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

                    other ->
                        other
            )

-}
accessors :
    { valuesRepresent : ValuesRepresent }
    -> VariantLensBuild
accessors { valuesRepresent } =
    \{ variantName, typeName, variantValues, typeParameters, variantModule, otherVariants } ->
        let
            qualifiedVariantName =
                [ variantModule, ".", variantName ]
                    |> String.concat

            { implementation, typeVariantValues } =
                if otherVariants |> Dict.isEmpty then
                    let
                        { access, alter, typeValues } =
                            variantOnly
                                { name = variantName
                                , values = variantValues
                                , valuesRepresent = valuesRepresent
                                }
                    in
                    { implementation =
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
                            variantAmongMultiple
                                { name = variantName
                                , values = variantValues
                                , valuesRepresent = valuesRepresent
                                }
                    in
                    { implementation =
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
                    ([ "Accessor lens for the variant `"
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


{-| [`VariantLensBuild`](#VariantLensBuild)
of unnamed [bChiquet/elm-accessors](https://dark.elm.dmy.fr/packages/bChiquet/elm-accessors/latest/)
which with

    { build =
        VariantLens.GenerateUsed.accessorsBChiquet
            { valuesRepresent = VariantLens.GenerateUsed.tupleNest }
    , name = VariantLens.GenerateUsed.prismNameVariant
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

    {-| Accessor lens for the variant `Data.Some` of the `type Data`.
    -}
    some :
        Relation ( a, ( b, ( c, d ) ) ) reachable wrap
        -> Relation (Data a b c d) reachable (Maybe wrap)
    some =
        makeOneToN
            (\variantValuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter |> Just

                    _ ->
                        Nothing
            )
            (\variantValuesAlter variantType ->
                case variantType of
                    Some value0 value1 value2 value3 ->
                        let
                            ( alteredValue0, ( alteredValue1, ( alteredValue2, alteredValue3 ) ) ) =
                                ( value0, ( value1, ( value2, value3 ) ) ) |> variantValuesAlter
                        in
                        Some alteredValue0 alteredValue1 alteredValue2 alteredValue3

                    other ->
                        other
            )

-}
accessorsBChiquet :
    { valuesRepresent : ValuesRepresent }
    -> VariantLensBuild
accessorsBChiquet { valuesRepresent } =
    \{ variantName, typeName, variantValues, typeParameters, variantModule, otherVariants } ->
        let
            { implementation, typeVariantValues } =
                if otherVariants |> Dict.isEmpty then
                    let
                        { access, alter, typeValues } =
                            variantOnly
                                { name = variantName
                                , values = variantValues
                                , valuesRepresent = valuesRepresent
                                }
                    in
                    { implementation = CodeGen.construct "makeOneToOne" [ access, alter ]
                    , typeVariantValues = typeValues
                    }

                else
                    let
                        { access, alter, typeValues } =
                            variantAmongMultiple
                                { name = variantName
                                , values = variantValues
                                , valuesRepresent = valuesRepresent
                                }
                    in
                    { implementation = CodeGen.construct "makeOneToN" [ access, alter ]
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
                    ([ "Accessor lens for the variant `"
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


{-| How to derive lens name <=> variant name.

Out of the box, there are

  - [`prismNameOnVariant`](prismNameOnVariant)
  - [`prismNameVariant`](#prismNameVariant)

You can also create a custom [`VariantLensNameConfig`](#VariantLensNameConfig):

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

Don't worry about the case of the results.
They will be automatically be corrected when passed to [`rule`](#rule).

-}
type alias VariantLensNameConfig =
    RecordWithoutConstructorFunction
        { parser : Parser { variantName : String }
        , build : { variantName : String } -> String
        }


{-| Handle lens names in the format `on<Variant>`.
Check out [`VariantLensNameConfig`](#VariantLensNameConfig) for all naming options.

    import Parser
    import VariantLens.GenerateUsed

    "onSuccess"
        |> Parser.run VariantLens.GenerateUsed.prismNameOnVariant.parser
    --> { variantName = "Success" }

    { variantName = "Success" }
        |> VariantLens.GenerateUsed.prismOnVariant.build
    --> "onSuccess"

-}
prismNameOnVariant : VariantLensNameConfig
prismNameOnVariant =
    { build = \{ variantName } -> "on" ++ variantName
    , parser =
        Parser.succeed (\variantName -> { variantName = variantName })
            |. Parser.token "on"
            |= (Parser.chompWhile (\_ -> True)
                    |> Parser.getChompedString
               )
    }


{-| Handle lens names in the format `on<Variant>`.
Check out [`VariantLensNameConfig`](#VariantLensNameConfig) for all naming options.

    import Parser
    import VariantLens.GenerateUsed

    "success"
        |> Parser.run VariantLens.GenerateUsed.prismNameOnVariant.parser
    --> { variantName = "Success" }

    { variantName = "Success" }
        |> VariantLens.GenerateUsed.prismOnVariant.build
    --> "success"

-}
prismNameVariant : VariantLensNameConfig
prismNameVariant =
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


{-| [Configure](#Config)
how to generate a variant lens declaration
plus the necessary `import`s.

Out of the box, there are

  - [`accessors`](#accessors)
  - [`accessorsBChiquet`](#accessorsBChiquet)

Customize with

  - [`documented`](#documented)
  - [`annotated`](#annotated)
  - [`importsAdd`](#importsAdd)

Create a custom lens generator (parts) with

  - [`the-sett/elm-syntax-dsl`](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest)
  - [`variantAmongMultiple`](#variantAmongMultiple), [`variantOnly`](#variantOnly)
  - [`tupleNest`](#tupleNest) , [`valuesRecord`](#valuesRecord)
  - [`variantPattern`](#variantPattern)

You can use the source code of [`accessors`](#accessors) & co. as a starting point.

-}
type alias VariantLensBuild =
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


{-| [Build](#VariantLensBuild) a different documentation:

    accessorsDocumentedCustom info =
        accessors
            { valuesRepresent = valuesRecord }
            info
            |> documented
                (emptyDocComment
                    |> markdown
                        ("variant `" ++ info.variantName ++ "`: Accessor lens for the values.")
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


{-| [Build](#VariantLensBuild) a different type annotation:

    import Hand exposing (Hand(..))
    import Stack

    accessorsAnnotatedLens : VariantLensBuild
    accessorsAnnotatedLens info =
        accessors
            { valuesRepresent = valuesRecord }
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

    accessorsAnnotatedLens : VariantLensBuild
    accessorsAnnotatedLens info =
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


ruleImplementation : Config -> Rule
ruleImplementation config =
    Rule.newProjectRuleSchema
        "VariantLens.GenerateUsed"
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
                                    { nameParser = config.name.parser
                                    , generationModuleSuffix = config.generationModuleSuffix
                                    , expressionNode = expressionNode
                                    }
                            )
                        )
                    |> Rule.withDeclarationEnterVisitor
                        (\(Node _ declaration) context ->
                            ( []
                            , context
                                |> declarationVisit
                                    { lensNameParser = config.name.parser
                                    , declaration = declaration
                                    }
                            )
                        )
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
                , uses :
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


type ModuleContext
    = -- `module` where variant prisms aren't generated in
      PossibleUseModuleContext PossibleUseModuleContext
    | -- `module` where variant prisms are generated in
      GenerationModuleContext GenerationModuleContext


type alias PossibleUseModuleContext =
    RecordWithoutConstructorFunction
        { moduleOriginLookup : ModuleNameLookupTable
        , uses :
            Dict
                -- by variant origin module without the generation module suffix
                String
                (Dict String Range)
        , variantTypes :
            Dict
                String
                { variants : Dict String (List CodeGen.TypeAnnotation)
                , parameters : List String
                }
        , -- the location to insert possible declarations first, then `import`s
          belowImportsRow : Int
        , importedModules : Set String
        }


type alias GenerationModuleContext =
    RecordWithoutConstructorFunction
        { variantModule : String
        , available :
            Set
                -- variant name
                String
        , exposing_ : Node Exposing
        , -- the location to insert possible declarations first, then `import`s
          belowImportsRow : Int
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
        (\meta moduleOriginLookup _ ->
            let
                moduleName =
                    meta |> Rule.moduleNameFromMetadata |> qualifiedSyntaxToString
            in
            case moduleName |> Parser.run (beforeSuffixParser ("." ++ generationModuleSuffix)) of
                Ok variantModule ->
                    initGenerationModuleContext { variantModule = variantModule }
                        |> GenerationModuleContext

                Err _ ->
                    initPossibleUseModuleContext { moduleOriginLookup = moduleOriginLookup }
                        |> PossibleUseModuleContext
        )
        |> Rule.withMetadata
        |> Rule.withModuleNameLookupTable


initGenerationModuleContext : { variantModule : String } -> GenerationModuleContext
initGenerationModuleContext { variantModule } =
    { variantModule = variantModule
    , belowImportsRow = 2
    , exposing_ =
        -- dummy. elm-review doesn't allow context change after visit
        Exposing.All Range.emptyRange |> Node Range.emptyRange
    , available = Set.empty
    }


initPossibleUseModuleContext :
    { moduleOriginLookup : ModuleNameLookupTable }
    -> PossibleUseModuleContext
initPossibleUseModuleContext { moduleOriginLookup } =
    { moduleOriginLookup = moduleOriginLookup
    , variantTypes = Dict.empty
    , uses = Dict.empty
    , belowImportsRow = 2
    , importedModules = Set.empty
    }


moduleContextToProject :
    { generationModuleSuffix : String }
    -> Rule.ContextCreator ModuleContext ProjectContext
moduleContextToProject { generationModuleSuffix } =
    Rule.initContextCreator
        (\meta moduleKey moduleContext ->
            case moduleContext of
                GenerationModuleContext generationModuleContext ->
                    { variantTypes = Dict.empty
                    , useModules = Dict.empty
                    , generationModules =
                        Dict.singleton
                            generationModuleContext.variantModule
                            { key = moduleKey
                            , exposing_ = generationModuleContext.exposing_
                            , belowImportsRow = generationModuleContext.belowImportsRow
                            , available = generationModuleContext.available
                            }
                    }

                PossibleUseModuleContext possibleUseModuleContext ->
                    let
                        moduleName =
                            meta |> Rule.moduleNameFromMetadata |> qualifiedSyntaxToString
                    in
                    { variantTypes =
                        Dict.singleton
                            moduleName
                            possibleUseModuleContext.variantTypes
                    , generationModules = Dict.empty
                    , useModules =
                        Dict.singleton
                            moduleName
                            { key = moduleKey
                            , belowImportsRow = possibleUseModuleContext.belowImportsRow
                            , uses =
                                possibleUseModuleContext.uses
                                    |> Dict.map
                                        (\variantModule variantsOfUses ->
                                            let
                                                generationModule =
                                                    variantModule ++ "." ++ generationModuleSuffix
                                            in
                                            { variantsOfUses = variantsOfUses
                                            , import_ =
                                                if Set.member generationModule possibleUseModuleContext.importedModules then
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


moduleHeaderVisit : Module -> ModuleContext -> ModuleContext
moduleHeaderVisit moduleHeader =
    \context ->
        case context of
            PossibleUseModuleContext possibleUseModuleContext ->
                possibleUseModuleContext |> PossibleUseModuleContext

            GenerationModuleContext generationModuleContext ->
                generationModuleContext
                    |> generationModuleContextModuleHeaderVisit moduleHeader
                    |> GenerationModuleContext


generationModuleContextModuleHeaderVisit : Module -> GenerationModuleContext -> GenerationModuleContext
generationModuleContextModuleHeaderVisit moduleHeader =
    case moduleHeader of
        Module.NormalModule { exposingList } ->
            \context -> { context | exposing_ = exposingList }

        Module.PortModule _ ->
            identity

        Module.EffectModule _ ->
            identity


belowImportsRowUpdate :
    Node Import
    -> { context | belowImportsRow : Int }
    -> { context | belowImportsRow : Int }
belowImportsRowUpdate importNode =
    \contextWithBelowImportsRow ->
        { contextWithBelowImportsRow
            | belowImportsRow =
                max contextWithBelowImportsRow.belowImportsRow
                    ((importNode |> Node.range |> .end |> .row) + 1)
        }


importVisit : Node Import -> ModuleContext -> ModuleContext
importVisit importNode =
    \context ->
        case context of
            GenerationModuleContext generationModuleContext ->
                generationModuleContext
                    |> belowImportsRowUpdate importNode
                    |> GenerationModuleContext

            PossibleUseModuleContext possibleUseModuleContext ->
                possibleUseModuleContext
                    |> possibleUseModuleImportVisit importNode
                    |> PossibleUseModuleContext


possibleUseModuleImportVisit :
    Node Import
    -> PossibleUseModuleContext
    -> PossibleUseModuleContext
possibleUseModuleImportVisit importNode =
    \possibleUseModuleContext ->
        let
            belowImportsRowUpdated =
                possibleUseModuleContext
                    |> belowImportsRowUpdate importNode
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
    { nameParser : Parser { variantName : String }
    , generationModuleSuffix : String
    , expressionNode : Node Expression
    }
    -> ModuleContext
    -> ModuleContext
expressionVisit info =
    \context ->
        case context of
            GenerationModuleContext generationModuleContext ->
                generationModuleContext |> GenerationModuleContext

            PossibleUseModuleContext possibleUseModuleContext ->
                possibleUseModuleContext
                    |> possibleUseModuleExpressionVisit info
                    |> PossibleUseModuleContext


possibleUseModuleExpressionVisit :
    { nameParser : Parser { variantName : String }
    , generationModuleSuffix : String
    , expressionNode : Node Expression
    }
    -> PossibleUseModuleContext
    -> PossibleUseModuleContext
possibleUseModuleExpressionVisit { nameParser, expressionNode, generationModuleSuffix } =
    case expressionNode |> Node.value of
        Expression.FunctionOrValue qualificationSyntax name ->
            case name |> Parser.run nameParser of
                Err _ ->
                    identity

                Ok { variantName } ->
                    \context ->
                        let
                            functionOrValueRange =
                                expressionNode |> Node.range

                            possibleGenerationModule =
                                ModuleNameLookupTable.moduleNameAt
                                    context.moduleOriginLookup
                                    functionOrValueRange
                                    |> -- not imported
                                       Maybe.withDefault qualificationSyntax
                                    |> qualifiedSyntaxToString
                        in
                        case possibleGenerationModule |> Parser.run (beforeSuffixParser ("." ++ generationModuleSuffix)) of
                            Err _ ->
                                context

                            Ok variantModule ->
                                { context
                                    | uses =
                                        context.uses
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
    { lensNameParser : Parser { variantName : String }
    , declaration : Declaration
    }
    -> ModuleContext
    -> ModuleContext
declarationVisit { lensNameParser, declaration } =
    \context ->
        case context of
            GenerationModuleContext generationModuleContext ->
                generationModuleContext
                    |> generationModuleDeclarationVisit
                        { lensNameParser = lensNameParser
                        , declaration = declaration
                        }
                    |> GenerationModuleContext

            PossibleUseModuleContext possibleUseModuleContext ->
                possibleUseModuleContext
                    |> possibleUseModuleDeclarationVisit declaration
                    |> PossibleUseModuleContext


generationModuleDeclarationVisit :
    { lensNameParser : Parser { variantName : String }
    , declaration : Declaration
    }
    -> GenerationModuleContext
    -> GenerationModuleContext
generationModuleDeclarationVisit { lensNameParser, declaration } =
    case declaration of
        Declaration.FunctionDeclaration functionDeclaration ->
            let
                name =
                    functionDeclaration.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            case name |> Parser.run lensNameParser of
                Err _ ->
                    identity

                Ok { variantName } ->
                    \generationModuleContext ->
                        { generationModuleContext
                            | available =
                                generationModuleContext.available
                                    |> Set.insert variantName
                        }

        _ ->
            identity


possibleUseModuleDeclarationVisit :
    Declaration
    -> PossibleUseModuleContext
    -> PossibleUseModuleContext
possibleUseModuleDeclarationVisit declaration =
    case declaration |> declarationToVariantType of
        Nothing ->
            identity

        Just ( variantTypeName, variantType ) ->
            \possibleUseModuleContext ->
                { possibleUseModuleContext
                    | variantTypes =
                        possibleUseModuleContext.variantTypes
                            |> Dict.insert variantTypeName variantType
                }


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
    { config : Config
    , context : ProjectContext
    }
    -> List (Rule.Error { useErrorForModule : () })
generateForProject { context, config } =
    context.useModules
        |> Dict.values
        |> List.concatMap
            (\useModule ->
                useModule.uses
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
    , config : Config
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
                    [ "variant lens generation `module ", generationModuleName, "` missing" ]
                        |> String.concat
                , details =
                    [ "Create such an elm file where variant prisms will be generated in."
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
                            [ "Add the variant lens generation `module` `import` through the supplied fix."
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
                                            config.name.build { variantName = variantName }
                                    in
                                    Rule.errorForModuleWithFix
                                        generationModule.key
                                        { message =
                                            [ "variant lens on `", variantModuleName, ".", variantName, "` missing" ]
                                                |> String.concat
                                        , details =
                                            [ "A variant lens with this name is used in other `module`s."
                                            , "Add the generated lens declaration through the fix."
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
                                                |> prismDeclarationToCodeGen
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
                                             ]
                                                |> String.concat
                                            )
                                        ]
                                )
                    )
            ]
                |> List.concat
