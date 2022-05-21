module VariantPrism.GenerateUsed exposing
    ( rule
    , accessors
    , VariantPrismGenerator, VariantPrismDeclaration, implementation, withDocumentation, withName
    )

{-|

@docs rule


# prism generators


## working out of the box

@docs accessors


## custom

@docs VariantPrismGenerator, VariantPrismDeclaration, implementation, withDocumentation, withName

-}

import Dict exposing (Dict)
import Elm.CodeGen as CodeGen
import Elm.Pretty as CodeGen
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Help exposing (char0ToLower, indexed, moduleNameToString, onRow)
import Parser
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import Stack
import VariantPrism.GenerateUsed.Testable exposing (beforeDotSuffixParser)


{-| Generate prisms for variant `type`s
that are called from your code but aren't already defined in a dedicated `module`.

    import Review.Rule as Rule exposing (Rule)
    import VariantPrism.GenerateUsed

    config : List Rule
    config =
        [ VariantPrism.GenerateUsed.rule
            { generator = VariantPrism.GenerateUsed.accessors
            , generationModuleSuffix = "On"
            }
        ]

`generationModuleSuffix = "On"` means that prisms are generated like `YourVariantType.On.variantName`. Choose the suffix you like best:

  - `RemoteData.Variant.success`
  - `RemoteData.Generated.onSuccess`
  - `RemoteData.X.onSuccess`
  - `RemoteData.On.success`
  - ...


## use it

... when you're using `elm-accessors` to mitigate
boilerplate related to updating potentially deeply nested data.


## don't use it

... when you consider lenses the less readable/intuitive/simple/explicit alternative.

-}
rule :
    { generator : VariantPrismGenerator
    , generationModuleSuffix : String
    }
    -> Rule
rule { generator, generationModuleSuffix } =
    Rule.newProjectRuleSchema "NoMissingVariantPrism"
        initialProjectContext
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
                            , context |> expressionVisit expressionNode
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
                projectContextToModule { generationModuleSuffix = generationModuleSuffix }
            , fromModuleToProject = moduleContextToProject
            , foldProjectContexts = projectContextsFold
            }
        |> Rule.withFinalProjectEvaluation
            (\context ->
                generatePrisms
                    { context = context
                    , generator = generator
                    , generationModuleSuffix = generationModuleSuffix
                    }
            )
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { baseModules :
        Dict
            String
            { key : Rule.ModuleKey
            , variantTypes :
                Dict
                    String
                    { variants :
                        Dict
                            String
                            { range : Range
                            , values : List CodeGen.TypeAnnotation
                            }
                    , parameters : List String
                    }
            }
    , useModules :
        Dict
            String
            { key : Rule.ModuleKey
            , belowImportsColumn : Int
            , uses :
                Dict
                    -- by base module without the generationModuleSuffix
                    String
                    { import_ : Presence
                    , variantsOfUses : Dict String Range
                    }
            }
    , generationModules :
        Dict
            -- by base module without the generationModuleSuffix
            String
            { -- the location to insert exposed prisms
              exposing_ : Node Exposing
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
                -- by base module without the generationModuleSuffix
                String
                (Dict String Range)
        , variantTypes :
            Dict
                String
                { variants :
                    Dict
                        String
                        { range : Range
                        , values : List CodeGen.TypeAnnotation
                        }
                , parameters : List String
                }

        -- the location to insert possible declarations first, then `import`s
        , belowImportsColumn : Int
        , importedModules : Set String
        }
    | GenerationModuleContext
        { baseModule : String
        , available : Set String
        , -- the location to insert exposed prisms
          exposing_ : Node Exposing
        , -- the location to insert possible declarations first, then `import`s
          belowImportsColumn : Int
        }


initialProjectContext : ProjectContext
initialProjectContext =
    { baseModules = Dict.empty
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
                    meta |> Rule.moduleNameFromMetadata |> moduleNameToString
            in
            case moduleName |> Parser.run (beforeDotSuffixParser generationModuleSuffix) of
                Ok baseModule ->
                    GenerationModuleContext
                        { baseModule = baseModule
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


moduleContextToProject : Rule.ContextCreator ModuleContext ProjectContext
moduleContextToProject =
    Rule.initContextCreator
        (\meta moduleKey moduleContext ->
            case moduleContext of
                GenerationModuleContext generationModuleContext ->
                    { baseModules = Dict.empty
                    , useModules = Dict.empty
                    , generationModules =
                        Dict.singleton
                            (meta |> Rule.moduleNameFromMetadata |> moduleNameToString)
                            { key = moduleKey
                            , exposing_ = generationModuleContext.exposing_
                            , belowImportsColumn = generationModuleContext.belowImportsColumn
                            , available = generationModuleContext.available
                            }
                    }

                PossibleUseModuleContext notGenerationModuleContext ->
                    let
                        moduleName =
                            meta |> Rule.moduleNameFromMetadata |> moduleNameToString
                    in
                    { useModules =
                        Dict.singleton moduleName
                            { key = moduleKey
                            , belowImportsColumn = notGenerationModuleContext.belowImportsColumn
                            , uses =
                                notGenerationModuleContext.uses
                                    |> Dict.map
                                        (\baseModule variantsOfUsesOfBaseModule ->
                                            { variantsOfUses = variantsOfUsesOfBaseModule
                                            , import_ =
                                                if Set.member baseModule notGenerationModuleContext.importedModules then
                                                    Present

                                                else
                                                    Missing
                                            }
                                        )
                            }
                    , baseModules = Dict.empty
                    , generationModules = Dict.empty
                    }
        )
        |> Rule.withMetadata
        |> Rule.withModuleKey


projectContextsFold : ProjectContext -> ProjectContext -> ProjectContext
projectContextsFold context0 context1 =
    { baseModules =
        context0.baseModules
            |> Dict.union context1.baseModules
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
            PossibleUseModuleContext notGenerationModuleContext ->
                notGenerationModuleContext |> PossibleUseModuleContext

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
            PossibleUseModuleContext notGenerationModuleContext ->
                notGenerationModuleContext
                    |> belowImportsColumnUpdate
                    |> (\r ->
                            { r
                                | importedModules =
                                    r.importedModules
                                        |> Set.insert
                                            (importNode
                                                |> Node.value
                                                |> .moduleName
                                                |> Node.value
                                                |> moduleNameToString
                                            )
                            }
                       )
                    |> PossibleUseModuleContext

            GenerationModuleContext generationModuleContext ->
                generationModuleContext
                    |> belowImportsColumnUpdate
                    |> GenerationModuleContext


expressionVisit : Node Expression -> ModuleContext -> ModuleContext
expressionVisit expressionNode =
    \context ->
        case context of
            GenerationModuleContext generationModuleContext ->
                generationModuleContext |> GenerationModuleContext

            PossibleUseModuleContext notGenerationModuleContext ->
                let
                    (Node range expression) =
                        expressionNode
                in
                (case expression of
                    Expression.FunctionOrValue qualification name ->
                        let
                            update moduleOrigin =
                                { notGenerationModuleContext
                                    | uses =
                                        notGenerationModuleContext.uses
                                            |> Dict.update
                                                (moduleOrigin |> moduleNameToString)
                                                (\usesSoFar ->
                                                    usesSoFar
                                                        |> Maybe.withDefault Dict.empty
                                                        |> Dict.insert name range
                                                        |> Just
                                                )
                                }
                        in
                        case
                            range
                                |> ModuleNameLookupTable.moduleNameAt
                                    notGenerationModuleContext.moduleOriginLookup
                        of
                            Just [] ->
                                notGenerationModuleContext

                            Just (moduleOriginPart0 :: moduleOriginPartsFrom1) ->
                                update (moduleOriginPart0 :: moduleOriginPartsFrom1)

                            -- not imported
                            Nothing ->
                                update qualification

                    _ ->
                        notGenerationModuleContext
                )
                    |> PossibleUseModuleContext


declarationVisit : Declaration -> ModuleContext -> ModuleContext
declarationVisit declaration =
    \context ->
        case context of
            GenerationModuleContext generationModuleContext ->
                (case declaration of
                    Declaration.FunctionDeclaration functionDeclaration ->
                        { generationModuleContext
                            | available =
                                generationModuleContext.available
                                    |> Set.insert
                                        (functionDeclaration.declaration
                                            |> Node.value
                                            |> .name
                                            |> Node.value
                                        )
                        }

                    _ ->
                        generationModuleContext
                )
                    |> GenerationModuleContext

            PossibleUseModuleContext notGenerationModuleContext ->
                (case declaration of
                    Declaration.CustomTypeDeclaration type_ ->
                        case type_.constructors of
                            [ _ ] ->
                                notGenerationModuleContext

                            _ :: _ :: _ ->
                                { notGenerationModuleContext
                                    | variantTypes =
                                        notGenerationModuleContext.variantTypes
                                            |> Dict.insert
                                                (type_.name |> Node.value)
                                                { parameters =
                                                    type_.generics
                                                        |> List.map Node.value
                                                , variants =
                                                    type_.constructors
                                                        |> List.map
                                                            (\(Node variantRange variant) ->
                                                                ( variant.name |> Node.value
                                                                , { values =
                                                                        variant.arguments
                                                                            |> List.map Node.value
                                                                  , range = variantRange
                                                                  }
                                                                )
                                                            )
                                                        |> Dict.fromList
                                                }
                                }

                            -- shouldn't parse
                            [] ->
                                notGenerationModuleContext

                    _ ->
                        notGenerationModuleContext
                )
                    |> PossibleUseModuleContext


generatePrisms :
    { generator : VariantPrismGenerator
    , generationModuleSuffix : String
    , context : ProjectContext
    }
    -> List (Rule.Error { useErrorForModule : () })
generatePrisms { context, generator, generationModuleSuffix } =
    context.useModules
        |> Dict.values
        |> List.concatMap
            (\useModule ->
                useModule.uses
                    |> Dict.toList
                    |> List.concatMap
                        (\( baseModuleName, usedBaseModule ) ->
                            case context.baseModules |> Dict.get baseModuleName of
                                Nothing ->
                                    []

                                Just baseModule ->
                                    let
                                        firstUseRange =
                                            usedBaseModule.variantsOfUses
                                                |> Dict.values
                                                |> List.head
                                                -- not expected
                                                |> Maybe.withDefault Range.emptyRange
                                    in
                                    case context.generationModules |> Dict.get baseModuleName of
                                        Nothing ->
                                            [ Rule.errorForModule useModule.key
                                                { message =
                                                    [ "missing generation `module ", baseModuleName, ".", generationModuleSuffix, "`" ]
                                                        |> String.concat
                                                , details =
                                                    [ "Create this elm file where variant prisms will be generated in." ]
                                                }
                                                firstUseRange
                                            ]

                                        Just generationModule ->
                                            [ case usedBaseModule.import_ of
                                                Present ->
                                                    []

                                                Missing ->
                                                    [ Rule.errorForModuleWithFix useModule.key
                                                        { message =
                                                            [ "missing `import ", baseModuleName, ".", generationModuleSuffix, "`" ]
                                                                |> String.concat
                                                        , details =
                                                            [ "Add the variant prism generation `module` `import` through the supplied fix" ]
                                                        }
                                                        firstUseRange
                                                        [ [ "\n"
                                                          , [ CodeGen.importStmt [ baseModuleName, generationModuleSuffix ] Nothing Nothing ]
                                                                |> CodeGen.prettyImports
                                                                |> pretty 1000
                                                          ]
                                                            |> String.concat
                                                            |> Fix.insertAt (useModule.belowImportsColumn |> onRow 1)
                                                        ]
                                                    ]
                                            , baseModule.variantTypes
                                                |> Dict.toList
                                                |> List.concatMap
                                                    (\( variantTypeName, variantType ) ->
                                                        variantType.variants
                                                            |> Dict.filter
                                                                (\variantName _ ->
                                                                    Dict.member variantName usedBaseModule.variantsOfUses
                                                                )
                                                            |> Dict.toList
                                                            |> List.map
                                                                (\( variantName, variant ) ->
                                                                    let
                                                                        generated =
                                                                            generator
                                                                                { variantModule = baseModuleName
                                                                                , typeName = variantTypeName
                                                                                , typeParameters = variantType.parameters
                                                                                , variantName = variantName
                                                                                , variantValues = variant.values
                                                                                }
                                                                    in
                                                                    Rule.errorForModuleWithFix generationModule.key
                                                                        { message =
                                                                            [ "missing prism for variant `", variantName, "`" ]
                                                                                |> String.concat
                                                                        , details =
                                                                            [ "A variant prism with this name is used in other `module`s."
                                                                            , "Add the generated prism declaration through the fix."
                                                                            ]
                                                                        }
                                                                        (generationModule.exposing_ |> Node.range)
                                                                        [ [ "\n\n"
                                                                          , generated.declaration
                                                                                |> prismDeclarationCodeGen
                                                                                |> CodeGen.prettyDeclaration 100
                                                                                |> pretty 100
                                                                          ]
                                                                            |> String.concat
                                                                            |> Fix.insertAt (generationModule.belowImportsColumn |> onRow 1)
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
                                                                        , [ "\n"
                                                                          , generated.imports
                                                                                |> (::)
                                                                                    (CodeGen.importStmt [ baseModuleName ]
                                                                                        Nothing
                                                                                        ([ CodeGen.openTypeExpose variantTypeName ]
                                                                                            |> CodeGen.exposeExplicit
                                                                                            |> Just
                                                                                        )
                                                                                    )
                                                                                |> CodeGen.prettyImports
                                                                                |> pretty 1000
                                                                          ]
                                                                            |> String.concat
                                                                            |> Fix.insertAt (generationModule.belowImportsColumn |> onRow 1)
                                                                        ]
                                                                )
                                                    )
                                            ]
                                                |> List.concat
                        )
            )


prismDeclarationCodeGen : VariantPrismDeclaration -> CodeGen.Declaration
prismDeclarationCodeGen =
    \prismDeclaration ->
        CodeGen.funDecl
            prismDeclaration.documentation
            prismDeclaration.annotation
            (prismDeclaration.name |> char0ToLower)
            []
            prismDeclaration.implementation



-- config


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


{-| Named [erlandsona/elm-accessors](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/) which

with `generationModuleSuffix = "On"` and

    module Data exposing (Data(..))

    type Data a b c d
        = Some a b c d
        | None

generates

    module Data.On exposing (some)

    import Accessors exposing (makeOneToN_)
    import Data exposing (Data(..))

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
accessors : VariantPrismGenerator
accessors =
    \{ variantName, typeName, variantValues, typeParameters, variantModule } ->
        { imports =
            [ CodeGen.importStmt [ "Accessors" ]
                Nothing
                ([ CodeGen.funExpose "makeOneToN_" ]
                    |> CodeGen.exposeExplicit
                    |> Just
                )
            ]
        , declaration =
            { documentation =
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
            , name = variantName
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
        }


{-| How to generate a [`VariantPrismDeclaration`](#VariantPrismDeclaration) plus the necessary `import`s.

Out of the box there are lenses for

  - [`elm-accessors`](#accessors)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    customVariantPrism : VariantPrismGenerator
    customVariantPrism { ... } =
        { imports =
            [ importStmt [ "CustomPrism" ]
                Nothing
                (exposeExplicit
                    [ typeOrAliasExpose "CustomPrism" ]
                    |> Just
                )
            ]
        , declaration = ...
        }

→ for `declaration`, see [`VariantPrismDeclaration`](#VariantPrismDeclaration)

-}
type alias VariantPrismGenerator =
    { variantModule : String
    , typeName : String
    , typeParameters : List String
    , variantName : String
    , variantValues : List CodeGen.TypeAnnotation
    }
    ->
        { declaration : VariantPrismDeclaration
        , imports : List CodeGen.Import
        }


{-| All the components to build a field lens declaration:

    {-| [documentation]
    -}
    [name] : [annotation]
    [name] =
        [implementation]

You can customize existing `VariantPrismDeclaration`s with [`withDocumentation`](#withDocumentation) and [`withName`](#withName)
or create custom prism ([`implementation`](#implementation) can be helpful).

    customPrismDeclaration { variantName, typeName, typeParameters, variantValues } =
        { documentation =
            emptyDocComment
                |> markdown
                    ("`CustomPrism` for the variant `" ++ variantName ++ "`.")
                |> Just
        , name = variantName
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

names will be [decapitalized](https://package.elm-lang.org/packages/elm-community/string-extra/latest/String-Extra#decapitalize).

-}
type alias VariantPrismDeclaration =
    { documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
    , name : String
    , annotation : Maybe CodeGen.TypeAnnotation
    , implementation : CodeGen.Expression
    }


{-| The provided [`VariantPrismGenerator`](#VariantPrismGenerator)s in this package have no documentation comment.

You can generate your own documentation, though:

    accessorsWithDocumentation { variantName } =
        accessors { variantName = variantName }
            |> withDocumentation
                (emptyDocComment
                    |> markdown
                        ("Accessor for the variant `" ++ variantName ++ "`.")
                )

-}
withDocumentation :
    CodeGen.Comment CodeGen.DocComment
    -> VariantPrismDeclaration
    -> VariantPrismDeclaration
withDocumentation docComment generatedFieldHelper =
    { generatedFieldHelper
        | documentation = docComment |> Just
    }


{-| Use a different name for the generated [`VariantPrismDeclaration`](#VariantPrismDeclaration).

    accessorPrisms { variantName } =
        { accessors
            | declaration =
                \info ->
                    accessors.declaration info
                        |> withName (info.variantName ++ "Prism")
        }

names will be [decapitalized](https://package.elm-lang.org/packages/elm-community/string-extra/latest/String-Extra#decapitalize).

-}
withName :
    String
    -> VariantPrismDeclaration
    -> VariantPrismDeclaration
withName name fieldHelperDeclaration =
    { fieldHelperDeclaration | name = name }
