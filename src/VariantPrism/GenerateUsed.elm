module VariantPrism.GenerateUsed exposing
    ( rule
    , VariantPrismBuild
    , accessors, accessorsBChiquet
    , documented, annotated, importsAdd
    , implementation
    , VariantPrismNameConfig, prismNameVariant, prismNameOnVariant
    )

{-|

@docs rule


## build

@docs VariantPrismBuild
@docs accessors, accessorsBChiquet
@docs documented, annotated, importsAdd
@docs implementation


## name

@docs VariantPrismNameConfig, prismNameVariant, prismNameOnVariant

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
import Help exposing (beforeSuffixParser, char0ToLower, char0ToUpper, declarationToString, importsToString, indexed, metaToVariantType, onRow, qualifiedSyntaxToString)
import Parser exposing ((|.), (|=), Parser)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import Stack
import VariantPrism.GenerateUsed.Testable exposing (prismDeclarationToCodeGen)


{-| Generate prisms for variant `type`s
that are called from your code but aren't already defined in a dedicated `module`.

    import Review.Rule as Rule exposing (Rule)
    import VariantPrism.GenerateUsed

    config : List Rule
    config =
        [ VariantPrism.GenerateUsed.rule ..config..
        ]

..config.. How to generate, where to generate:

  - `build :`
    a [`VariantPrismBuild` function](#VariantPrismBuild) like
      - [`accessors`](#accessors)
      - [`accessorsBChiquet`](#accessorsBChiquet)
  - `name :`
    a way to handle variant prism names like
      - [`prismNameOnVariant`](#prismNameOnVariant)
      - [`prismNameVariant`](#prismNameVariant)
  - `generationModuleIsVariantModuleDotSuffix :`
    a `.Suffix` to derive generation `module` names from variant `module` names

There's no configuration to automatically `import Variant.Module.Generation as Variant.Module`
because [`import` aliases can't contain `.`](https://github.com/elm/compiler/issues/2260)


### example `module Variant.Module.On exposing (some)`

    { build = VariantPrism.GenerateUsed.accessors
    , name = VariantPrism.GenerateUsed.prismNameVariant
    , generationModuleIsVariantModuleDotSuffix = "On"
    }


### example: `module Variant.Module.X exposing (onSome)`

    { build = VariantPrism.GenerateUsed.accessors
    , name = VariantPrism.GenerateUsed.prismNameOnVariant
    , generationModuleIsVariantModuleDotSuffix = "X"
    }


## use it

... when you're using `elm-accessors` to mitigate
boilerplate related to updating potentially deeply nested data.


## don't use it

... when you consider lenses the less readable/intuitive/simple/explicit alternative.

-}
rule :
    { name : VariantPrismNameConfig
    , build : VariantPrismBuild
    , generationModuleIsVariantModuleDotSuffix : String
    }
    -> Rule
rule config =
    ruleImplementation
        { build = config.build
        , name = config.name
        , generationModuleSuffix = config.generationModuleIsVariantModuleDotSuffix
        }



-- config


type alias Config =
    RecordWithoutConstructorFunction
        { name : VariantPrismNameConfig
        , build : VariantPrismBuild
        , generationModuleSuffix : String
        }


{-| Helpers for values of a given variant in the form

with

    type Data a b c d
        = Some a b c d
        | None

generates


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

            someNot ->
                someNot

-}
implementation :
    { variantName : String
    , variantValues : List CodeGen.TypeAnnotation
    }
    ->
        { access : CodeGen.Expression
        , alter : CodeGen.Expression
        }
implementation { variantName, variantValues } =
    { access =
        CodeGen.lambda
            [ CodeGen.varPattern "variantValuesAlter"
            , CodeGen.varPattern "variantType"
            ]
            (CodeGen.caseExpr (CodeGen.val "variantType")
                [ ( CodeGen.namedPattern variantName
                        (variantValues
                            |> List.indexedMap
                                (\index _ ->
                                    CodeGen.varPattern ("value" |> indexed index)
                                )
                        )
                  , CodeGen.binOpChain
                        (case variantValues of
                            [] ->
                                CodeGen.unit

                            top :: down ->
                                Stack.topDown top down
                                    |> Stack.map
                                        (\{ index } _ ->
                                            CodeGen.val ("value" |> indexed index)
                                        )
                                    |> Stack.fold (\value soFar -> CodeGen.tuple [ soFar, value ])
                        )
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
                [ ( CodeGen.namedPattern
                        variantName
                        (variantValues
                            |> List.indexedMap
                                (\index _ ->
                                    CodeGen.varPattern ("value" |> indexed index)
                                )
                        )
                  , case variantValues of
                        [] ->
                            CodeGen.binOpChain
                                CodeGen.unit
                                CodeGen.piper
                                [ CodeGen.fun "variantValuesAlter"
                                , CodeGen.construct "variantValuesCurryTo"
                                    [ CodeGen.fun variantName ]
                                ]

                        [ _ ] ->
                            CodeGen.construct variantName
                                [ CodeGen.construct "variantValueAlter"
                                    [ CodeGen.val ("value" |> indexed 0) ]
                                    |> CodeGen.parens
                                ]

                        -- >= 2
                        element0 :: element1 :: elements2Up ->
                            let
                                variantValuesStack =
                                    Stack.topDown element0 (element1 :: elements2Up)
                            in
                            CodeGen.letExpr
                                [ CodeGen.letDestructuring
                                    (variantValuesStack
                                        |> Stack.map
                                            (\{ index } _ ->
                                                CodeGen.varPattern ("alteredValue" |> indexed index)
                                            )
                                        |> Stack.fold (\value soFar -> CodeGen.tuplePattern [ soFar, value ])
                                    )
                                    (CodeGen.construct "variantTagValue"
                                        (variantValues
                                            |> List.indexedMap
                                                (\index _ ->
                                                    CodeGen.val ("value" |> indexed index)
                                                )
                                        )
                                    )
                                ]
                                (CodeGen.binOpChain
                                    (variantValuesStack
                                        |> Stack.map
                                            (\{ index } _ ->
                                                CodeGen.val ("value" |> indexed index)
                                            )
                                        |> Stack.fold (\value soFar -> CodeGen.tuple [ soFar, value ])
                                    )
                                    CodeGen.piper
                                    [ CodeGen.fun "variantValuesAlter"
                                    , CodeGen.construct variantName
                                        (variantValuesStack
                                            |> Stack.map
                                                (\{ index } _ ->
                                                    CodeGen.val ("alteredValue" |> indexed index)
                                                )
                                            |> Stack.toList
                                        )
                                    ]
                                )
                  )
                , let
                    variantNot =
                        (variantName |> char0ToLower) ++ "Not"
                  in
                  ( CodeGen.namedPattern variantNot []
                  , CodeGen.val variantNot
                  )
                ]
            )
    }


{-| [`VariantPrismBuild`](#VariantPrismBuild)
of named [erlandsona/elm-accessors](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/)
which with

    { build = VariantPrism.GenerateUsed.accessors
    , name = VariantPrism.GenerateUsed.prismNameVariant
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

    {-| Accessor prism for the variant `Some` of the `type Data`.
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

                    someNot ->
                        someNot
            )

-}
accessors : VariantPrismBuild
accessors =
    \{ variantName, typeName, variantValues, typeParameters, variantModule } ->
        { imports =
            [ CodeGen.importStmt [ "Accessors" ]
                Nothing
                ([ CodeGen.typeOrAliasExpose "Relation"
                 , CodeGen.funExpose "makeOneToN_"
                 ]
                    |> CodeGen.exposeExplicit
                    |> Just
                )
            ]
        , documentation =
            CodeGen.emptyDocComment
                |> CodeGen.markdown
                    ([ "Accessor prism for the variant `"
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
                (CodeGen.typed "Relation"
                    [ case variantValues of
                        [] ->
                            CodeGen.unitAnn

                        top :: down ->
                            Stack.topDown top down
                                |> Stack.fold (\value soFar -> CodeGen.tupleAnn [ soFar, value ])
                    , CodeGen.typeVar "reachable"
                    , CodeGen.typeVar "wrap"
                    ]
                )
                (CodeGen.typed "Relation"
                    [ CodeGen.typed typeName
                        (typeParameters |> List.map CodeGen.typeVar)
                    , CodeGen.typeVar "reachable"
                    , CodeGen.maybeAnn (CodeGen.typeVar "wrap")
                    ]
                )
                |> Just
        , implementation =
            let
                { access, alter } =
                    implementation
                        { variantName = variantName
                        , variantValues = variantValues
                        }
            in
            CodeGen.construct "makeOneToN_"
                [ CodeGen.string (variantModule ++ "." ++ variantName)
                , access
                , alter
                ]
        }


{-| [`VariantPrismBuild`](#VariantPrismBuild)
of named [erlandsona/elm-accessors](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/)
which with

    { build = VariantPrism.GenerateUsed.accessors
    , name = VariantPrism.GenerateUsed.prismNameVariant
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

    {-| Accessor prism for the variant `Some` of the `type Data`.
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

                    someNot ->
                        someNot
            )

-}
accessorsBChiquet : VariantPrismBuild
accessorsBChiquet =
    \{ variantName, typeName, variantValues, typeParameters } ->
        { imports =
            [ CodeGen.importStmt [ "Accessors" ]
                Nothing
                ([ CodeGen.typeOrAliasExpose "Relation"
                 , CodeGen.funExpose "makeOneToN"
                 ]
                    |> CodeGen.exposeExplicit
                    |> Just
                )
            ]
        , documentation =
            CodeGen.emptyDocComment
                |> CodeGen.markdown
                    ([ "Accessor prism for the variant `"
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
                    (case variantValues of
                        [] ->
                            CodeGen.unitAnn

                        top :: down ->
                            Stack.topDown top down
                                |> Stack.fold (\value soFar -> CodeGen.tupleAnn [ soFar, value ])
                    )
                    (CodeGen.typeVar "reachable")
                    (CodeGen.typeVar "wrap")
                )
                (typeRelation
                    (CodeGen.typed typeName
                        (typeParameters |> List.map CodeGen.typeVar)
                    )
                    (CodeGen.typeVar "reachable")
                    (CodeGen.maybeAnn (CodeGen.typeVar "wrap"))
                )
                |> Just
        , implementation =
            let
                { access, alter } =
                    implementation
                        { variantName = variantName
                        , variantValues = variantValues
                        }
            in
            CodeGen.construct "makeOneToN" [ access, alter ]
        }


typeRelation :
    CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
typeRelation structure attribute wrap =
    CodeGen.typed "Relation" [ structure, attribute, wrap ]


{-| How to derive prism name <=> variant name.

Out of the box, there are

  - [`prismNameOnVariant`](prismNameOnVariant)
  - [`prismNameVariant`](#prismNameVariant)

You can also create a custom [`VariantPrismNameConfig`](#VariantPrismNameConfig):

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
They will be automatically be corrected on [`inVariantOriginModuleDotSuffix`](#inVariantOriginModuleDotSuffix).

-}
type alias VariantPrismNameConfig =
    RecordWithoutConstructorFunction
        { parser : Parser { variantName : String }
        , build : { variantName : String } -> String
        }


{-| Handle prism names in the format `on<Variant>`.
Check out [`VariantPrismNameConfig`](#VariantPrismNameConfig) for all naming options.

    import Parser
    import VariantPrism.GenerateUsed

    "onSuccess"
        |> Parser.run VariantPrism.GenerateUsed.prismNameOnVariant.parser
    --> { variantName = "Success" }

    { variantName = "Success" }
        |> VariantPrism.GenerateUsed.prismOnVariant.build
    --> "onSuccess"

-}
prismNameOnVariant : VariantPrismNameConfig
prismNameOnVariant =
    { build = \{ variantName } -> "on" ++ variantName
    , parser =
        Parser.succeed (\variantName -> { variantName = variantName })
            |. Parser.token "on"
            |= (Parser.chompWhile (\_ -> True)
                    |> Parser.getChompedString
               )
    }


{-| Handle prism names in the format `on<Variant>`.
Check out [`VariantPrismNameConfig`](#VariantPrismNameConfig) for all naming options.

    import Parser
    import VariantPrism.GenerateUsed

    "success"
        |> Parser.run VariantPrism.GenerateUsed.prismNameOnVariant.parser
    --> { variantName = "Success" }

    { variantName = "Success" }
        |> VariantPrism.GenerateUsed.prismOnVariant.build
    --> "success"

-}
prismNameVariant : VariantPrismNameConfig
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


{-| How to generate a variant prism declaration plus the necessary `import`s.

Out of the box, there are

  - [`accessors`](#accessors)

You can customize existing variant prism declarations with [`documented`](#documented) and [`annotated`](#annotated)
or create a custom prism generator ([the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest), [`implementation`](#implementation) can be helpful).

    customPrismGenerator : VariantPrismBuild
    customPrismGenerator { variantName, typeName, typeParameters, variantValues } =
        { imports =
            [ importStmt [ "CustomPrism" ]
                Nothing
                (exposeExplicit
                    [ typeOrAliasExpose "CustomPrism" ]
                    |> Just
                )
                |> Just
            , case variantValues of
                [] ->
                    Nothing

                [ _ ] ->
                    Nothing

                _ :: _ :: valueTypeFrom2 ->
                    let
                        valueCount =
                            2 + (valueTypeFrom2 |> List.length)
                    in
                    importStmt [ "Toop", "T" ++ (valueCount |> String.fromInt) ]
                        Nothing
                        Nothing
            ]
                |> List.filterMap identity
        , documentation =
            emptyDocComment
                |> markdown
                    ("`CustomPrism` for the variant `" ++ variantName ++ "`.")
                |> Just
        , annotation =
            typed "CustomPrism"
                [ CodeGen.typed typeName
                    (typeParameters |> List.map CodeGen.typeVar)
                , case variantValues of
                    [] ->
                        unitAnn

                    [ singleValueType ] ->
                        singleValueType

                    valueType0 :: valueType1 :: valueTypeFrom2 ->
                        let
                            argumentCount =
                                2 + (valueTypeFrom2 |> List.length)
                        in
                        fqConstruct
                            [ "Toop" ]
                            ("T" ++ (argumentCount |> String.fromInt))
                            (valueType0 :: valueType1 :: valueTypeFrom2)
                , CodeGen.typeVar "wrap"
                , CodeGen.typeVar
                ]
                |> Just
        , implementation =
            let
                { access, alter } =
                    implementation
                        { variantName = variantName
                        , variantValues = variantValues
                        }
            in
            fqConstruct [ "CustomPrism" ] "create" [ access, alter ]
        }

Once you've chosen a `VariantPrismBuild`,
you can [configure](#Config) the [prism variant `name`](#PrismVariantNameConfig)
and after that [`inVariantOriginModuleDotSuffix`](#inVariantOriginModuleDotSuffix).

-}
type alias VariantPrismBuild =
    { variantModule : String
    , typeName : String
    , typeParameters : List String
    , variantName : String
    , variantValues : List CodeGen.TypeAnnotation
    }
    ->
        { imports : List CodeGen.Import
        , documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
        , annotation : Maybe CodeGen.TypeAnnotation
        , implementation : CodeGen.Expression
        }


{-| [Build](#VariantPrismBuild) a different documentation:

    accessorsDocumentedCustom info =
        accessors info
            |> documented
                (emptyDocComment
                    |> markdown
                        ("Accessor prism for the variant `" ++ info.variantName ++ "`.")
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


{-| [Build](#VariantPrismBuild) a different type annotation:

    accessorsAnnotatedPrism : VariantPrismBuild
    accessorsAnnotatedPrism info =
        accessors info
            |> annotated
                (typed "Prism"
                    [ CodeGen.typed info.typeName
                        (info.typeParameters |> List.map CodeGen.typeVar)
                    , case variantValues of
                        [] ->
                            CodeGen.unitAnn

                        top :: down ->
                            Stack.topDown top down
                                |> Stack.fold (\value soFar -> CodeGen.tupleAnn [ soFar, value ])
                    , CodeGen.typeVar "reachable"
                    , CodeGen.typeVar "wrap"
                    ]
                )
            |> importsAdd
                [ impostStmt [ "Accessors" ]
                    Nothing
                    ([ "Prism" |> typeOrAliasExpose ] |> exposingExplicit |> Just)
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

    accessorsAnnotatedPrism : VariantPrismBuild
    accessorsAnnotatedPrism info =
        accessors info
            |> annotated (typed "Prism" [ ... ])
            |> importsAdd
                [ impostStmt [ "Accessors" ]
                    Nothing
                    ([ "Prism" |> typeOrAliasExpose ] |> exposingExplicit |> Just)
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
        "NoMissingVariantPrism"
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
                            , context |> declarationVisit declaration
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
            , belowImportsColumn : Int
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
              belowImportsColumn : Int
            , available : Set String
            , key : Rule.ModuleKey
            }
    }


type Presence
    = Present
    | Missing


type ModuleContext
    = PossibleUseModuleContext
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
          belowImportsColumn : Int
        , importedModules : Set String
        }
    | GenerationModuleContext
        { originModule : String
        , available : Set String
        , exposing_ : Node Exposing
        , -- the location to insert possible declarations first, then `import`s
          belowImportsColumn : Int
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
                Ok originModule_ ->
                    GenerationModuleContext
                        { originModule = originModule_
                        , belowImportsColumn = 2
                        , exposing_ =
                            -- dummy. elm-review doesn't allow context change after visit
                            Exposing.All Range.emptyRange |> Node Range.emptyRange
                        , available = Set.empty
                        }

                Err _ ->
                    PossibleUseModuleContext
                        { moduleOriginLookup = moduleOriginLookup
                        , variantTypes = Dict.empty
                        , uses = Dict.empty
                        , belowImportsColumn = 2
                        , importedModules = Set.empty
                        }
        )
        |> Rule.withMetadata
        |> Rule.withModuleNameLookupTable


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
            case moduleContext of
                GenerationModuleContext generationModuleContext ->
                    { variantTypes = Dict.empty
                    , useModules = Dict.empty
                    , generationModules =
                        Dict.singleton
                            (meta |> Rule.moduleNameFromMetadata |> qualifiedSyntaxToString)
                            { key = moduleKey
                            , exposing_ = generationModuleContext.exposing_
                            , belowImportsColumn = generationModuleContext.belowImportsColumn
                            , available = generationModuleContext.available
                            }
                    }

                PossibleUseModuleContext possibleUseModuleContext ->
                    { variantTypes = Dict.empty
                    , generationModules = Dict.empty
                    , useModules =
                        Dict.singleton moduleName
                            { key = moduleKey
                            , belowImportsColumn = possibleUseModuleContext.belowImportsColumn
                            , uses =
                                possibleUseModuleContext.uses
                                    |> Dict.map
                                        (\originModule_ variantsOfUsesOfVariantOriginModule ->
                                            let
                                                generationModuleName =
                                                    originModule_ ++ "." ++ generationModuleSuffix
                                            in
                                            { variantsOfUses = variantsOfUsesOfVariantOriginModule
                                            , import_ =
                                                if Set.member generationModuleName possibleUseModuleContext.importedModules then
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
                case moduleHeader of
                    Module.NormalModule { exposingList } ->
                        { generationModuleContext
                            | exposing_ = exposingList
                        }
                            |> GenerationModuleContext

                    Module.PortModule _ ->
                        generationModuleContext |> GenerationModuleContext

                    Module.EffectModule _ ->
                        generationModuleContext |> GenerationModuleContext


importVisit : Node Import -> ModuleContext -> ModuleContext
importVisit importNode =
    \context ->
        let
            belowImportsColumnUpdate contextWithBelowImports =
                { contextWithBelowImports
                    | belowImportsColumn =
                        max contextWithBelowImports.belowImportsColumn
                            ((importNode |> Node.range |> .end |> .column) + 1)
                }
        in
        case context of
            GenerationModuleContext generationModuleContext ->
                generationModuleContext
                    |> belowImportsColumnUpdate
                    |> GenerationModuleContext

            PossibleUseModuleContext possibleUseModuleContext ->
                let
                    belowImportsColumnUpdated =
                        possibleUseModuleContext
                            |> belowImportsColumnUpdate
                in
                { belowImportsColumnUpdated
                    | importedModules =
                        belowImportsColumnUpdated.importedModules
                            |> Set.insert
                                (importNode
                                    |> Node.value
                                    |> .moduleName
                                    |> Node.value
                                    |> qualifiedSyntaxToString
                                )
                }
                    |> PossibleUseModuleContext


expressionVisit :
    { nameParser : Parser { variantName : String }
    , generationModuleSuffix : String
    , expressionNode : Node Expression
    }
    -> ModuleContext
    -> ModuleContext
expressionVisit { nameParser, expressionNode, generationModuleSuffix } =
    \context ->
        case context of
            GenerationModuleContext generationModuleContext ->
                generationModuleContext |> GenerationModuleContext

            PossibleUseModuleContext possibleUseModuleContext ->
                (case expressionNode |> Node.value of
                    Expression.FunctionOrValue qualificationSyntax name ->
                        case name |> Parser.run nameParser of
                            Err _ ->
                                possibleUseModuleContext

                            Ok { variantName } ->
                                let
                                    functionOrValueRange =
                                        expressionNode |> Node.range

                                    possibleGenerationModule =
                                        ModuleNameLookupTable.moduleNameAt
                                            possibleUseModuleContext.moduleOriginLookup
                                            functionOrValueRange
                                            |> Maybe.withDefault qualificationSyntax
                                            |> qualifiedSyntaxToString
                                in
                                case possibleGenerationModule |> Parser.run (beforeSuffixParser ("." ++ generationModuleSuffix)) of
                                    Err _ ->
                                        possibleUseModuleContext

                                    Ok originModule_ ->
                                        { possibleUseModuleContext
                                            | uses =
                                                possibleUseModuleContext.uses
                                                    |> Dict.update
                                                        originModule_
                                                        (\usesSoFar ->
                                                            usesSoFar
                                                                |> Maybe.withDefault Dict.empty
                                                                |> Dict.insert variantName functionOrValueRange
                                                                |> Just
                                                        )
                                        }

                    _ ->
                        possibleUseModuleContext
                )
                    |> PossibleUseModuleContext


declarationVisit : Declaration -> ModuleContext -> ModuleContext
declarationVisit declaration =
    \context ->
        case context of
            GenerationModuleContext generationModuleContext ->
                (case declaration of
                    Declaration.FunctionDeclaration functionDeclaration ->
                        let
                            (Node _ name) =
                                functionDeclaration.declaration |> Node.value |> .name
                        in
                        { generationModuleContext
                            | available =
                                generationModuleContext.available
                                    |> Set.insert name
                        }

                    _ ->
                        generationModuleContext
                )
                    |> GenerationModuleContext

            PossibleUseModuleContext possibleUseModuleContext ->
                (case declaration |> declarationToVariantType of
                    Nothing ->
                        possibleUseModuleContext

                    Just ( variantTypeName, variantType ) ->
                        { possibleUseModuleContext
                            | variantTypes =
                                possibleUseModuleContext.variantTypes
                                    |> Dict.insert variantTypeName variantType
                        }
                )
                    |> PossibleUseModuleContext


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


declarationToVariantType :
    Declaration
    ->
        Maybe
            ( String
            , { variants :
                    Dict String (List CodeGen.TypeAnnotation)
              , parameters : List String
              }
            )
declarationToVariantType =
    \declaration ->
        case declaration of
            Declaration.CustomTypeDeclaration type_ ->
                case type_.constructors of
                    -- shouldn't parse
                    [] ->
                        Nothing

                    -- in the future? add accessors for 1-constructor-`type`s
                    [ _ ] ->
                        Nothing

                    variant0 :: variant1 :: variantsFrom2 ->
                        ( type_.name |> Node.value
                        , { parameters =
                                type_.generics |> List.map Node.value
                          , variants =
                                (variant0 :: variant1 :: variantsFrom2)
                                    |> List.map
                                        (\(Node _ variant) ->
                                            ( variant.name |> Node.value
                                            , variant.arguments |> List.map Node.value
                                            )
                                        )
                                    |> Dict.fromList
                          }
                        )
                            |> Just

            _ ->
                Nothing


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
                                            context.generationModules |> Dict.get usedVariantOriginModuleName
                                        , usedVariantOriginModule = usedVariantOriginModule
                                        , variantOriginModuleName = usedVariantOriginModuleName
                                        , config = config
                                        , useModuleKey = useModule.key
                                        , useModuleBelowImportsColumn = useModule.belowImportsColumn
                                        , variantTypes = variantTypesInModule
                                        }
                        )
            )


generateForModule :
    { usedVariantOriginModule :
        { variantsOfUses : Dict String Range, import_ : Presence }
    , useModuleKey : Rule.ModuleKey
    , useModuleBelowImportsColumn : Int
    , variantOriginModuleName : String
    , maybeGenerationModule :
        Maybe
            { exposing_ : Node Exposing
            , -- the location to insert possible declarations first, then `import`s
              belowImportsColumn : Int
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
generateForModule { usedVariantOriginModule, variantOriginModuleName, maybeGenerationModule, useModuleBelowImportsColumn, config, variantTypes, useModuleKey } =
    let
        firstUseRange =
            usedVariantOriginModule.variantsOfUses
                |> Dict.values
                |> List.head
                -- not expected
                |> Maybe.withDefault Range.emptyRange

        generationModuleName =
            [ variantOriginModuleName, ".", config.generationModuleSuffix ]
                |> String.concat
    in
    case maybeGenerationModule of
        Nothing ->
            [ Rule.errorForModule useModuleKey
                { message =
                    [ "missing generation `module ", generationModuleName, "`" ]
                        |> String.concat
                , details =
                    [ "Create this elm file where variant prisms will be generated in." ]
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
                            [ "Add the variant prism generation `module` `import` through the supplied fix."
                            , -- TODO remove
                              "!Test value!: variant origin name = `" ++ variantOriginModuleName ++ "`"
                            ]
                        }
                        firstUseRange
                        [ [ "\n", importString ]
                            |> String.concat
                            |> Fix.insertAt (useModuleBelowImportsColumn |> onRow 1)
                        ]
                    ]
            , variantTypes
                |> Dict.toList
                |> List.concatMap
                    (\( variantTypeName, variantType ) ->
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
                                                { variantModule = variantOriginModuleName
                                                , typeName = variantTypeName
                                                , typeParameters = variantType.parameters
                                                , variantName = variantName
                                                , variantValues = variantValues
                                                }
                                    in
                                    Rule.errorForModuleWithFix
                                        generationModule.key
                                        { message =
                                            [ "prism for variant `", variantName, "` missing" ]
                                                |> String.concat
                                        , details =
                                            [ "A variant prism with this name is used in other `module`s."
                                            , "Add the generated prism declaration through the fix."
                                            ]
                                        }
                                        (generationModule.exposing_ |> Node.range)
                                        [ Fix.insertAt
                                            (generationModule.belowImportsColumn |> onRow 1)
                                            ([ "\n\n"
                                             , { name = config.name.build { variantName = variantName }
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
                                                Fix.replaceRangeBy dotDotRange variantName

                                            Node exposeRange (Exposing.Explicit exposes) ->
                                                (variantName ++ ", ")
                                                    |> Fix.insertAt
                                                        (case exposes of
                                                            (Node firstExposeRange _) :: _ ->
                                                                firstExposeRange.start

                                                            -- exposing () shouldn't parse
                                                            [] ->
                                                                exposeRange.end
                                                        )
                                        , Fix.insertAt
                                            (generationModule.belowImportsColumn |> onRow 1)
                                            ([ "\n"
                                             , [ built.imports
                                               , [ CodeGen.importStmt
                                                    [ variantOriginModuleName ]
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
