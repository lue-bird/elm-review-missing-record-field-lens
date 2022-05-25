module NoMissingRecordFieldLens.Internal exposing (nonExistentFieldLensNameInfo, printFieldLensDeclaration, rule)

import Dict exposing (Dict)
import Elm.CodeGen as CodeGen
import Elm.Pretty as CodeGen
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location, Range)
import List.Extra as List
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (ContextCreator, ModuleKey, Rule)


rule : Config -> Rule
rule config =
    let
        { generator, generateIn } =
            config

        generationModule =
            let
                ( head, tail ) =
                    generateIn
            in
            head :: tail

        projectContextToModule : ContextCreator ProjectContext ModuleContext
        projectContextToModule =
            Rule.initContextCreator
                (\metadata moduleNameLookupTable _ ->
                    let
                        moduleName =
                            Rule.moduleNameFromMetadata metadata
                    in
                    if moduleName == generationModule then
                        GenerationModule
                            { content = Dict.empty
                            , beforeDeclarations = { row = 1, column = 1 }
                            , exposing_ = Exposing.Explicit [] |> Node Range.emptyRange
                            }

                    else
                        NotGenerationModule
                            { usages = Dict.empty
                            , moduleNameLookupTable = moduleNameLookupTable
                            }
                )
                |> Rule.withMetadata
                |> Rule.withModuleNameLookupTable

        moduleContextToProject : ContextCreator ModuleContext ProjectContext
        moduleContextToProject =
            Rule.initContextCreator
                (\metadata moduleKey moduleContext ->
                    case moduleContext of
                        GenerationModule { content, beforeDeclarations, exposing_ } ->
                            { modulesWithUsages = []
                            , generationModuleInfo =
                                { key = moduleKey
                                , moduleNameRange =
                                    Rule.moduleNameNodeFromMetadata metadata |> Node.range
                                , exposing_ = exposing_
                                , beforeDeclarations = beforeDeclarations
                                , content = content
                                }
                                    |> Just
                            }

                        NotGenerationModule { usages } ->
                            { modulesWithUsages =
                                [ { key = moduleKey
                                  , usages = usages
                                  }
                                ]
                            , generationModuleInfo = Nothing
                            }
                )
                |> Rule.withMetadata
                |> Rule.withModuleKey

        foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
        foldProjectContexts aContext bContext =
            { modulesWithUsages =
                [ aContext.modulesWithUsages
                , bContext.modulesWithUsages
                ]
                    |> List.concat
            , generationModuleInfo =
                [ aContext, bContext ]
                    |> List.filterMap .generationModuleInfo
                    |> List.head
            }

        initialProjectContext : ProjectContext
        initialProjectContext =
            { modulesWithUsages = []
            , generationModuleInfo = Nothing
            }

        finalEvaluation : ProjectContext -> List (Rule.Error errorScope_)
        finalEvaluation { modulesWithUsages, generationModuleInfo } =
            modulesWithUsages
                |> List.concatMap
                    (\moduleWithUsages ->
                        case generationModuleInfo of
                            Nothing ->
                                moduleWithUsages.usages
                                    |> Dict.values
                                    |> List.map
                                        (\range ->
                                            Rule.errorForModule
                                                moduleWithUsages.key
                                                (generationModuleDoesntExistInfo generationModule)
                                                range
                                        )

                            Just generationModule_ ->
                                generationModule_.content
                                    |> Dict.diff moduleWithUsages.usages
                                    |> Dict.keys
                                    |> List.map
                                        (\nonExistentFieldLensName ->
                                            Rule.errorForModuleWithFix
                                                generationModule_.key
                                                (nonExistentFieldLensNameInfo nonExistentFieldLensName)
                                                generationModule_.moduleNameRange
                                                (let
                                                    insertLocationInDeclarations =
                                                        generationModule_.content
                                                            |> Dict.toList
                                                            |> List.dropWhileRight
                                                                (\( existing, _ ) ->
                                                                    existing > nonExistentFieldLensName
                                                                )
                                                            |> List.head
                                                 in
                                                 [ [ Fix.insertAt
                                                        (case insertLocationInDeclarations of
                                                            Just ( _, range ) ->
                                                                range.end

                                                            Nothing ->
                                                                generationModule_.beforeDeclarations
                                                        )
                                                        ([ "\n\n\n"
                                                         , generator.declaration { fieldName = nonExistentFieldLensName }
                                                            |> printFieldLensDeclaration
                                                         ]
                                                            |> String.concat
                                                        )
                                                   , Fix.insertAt
                                                        (generationModule_.exposing_ |> Node.range |> .end)
                                                        ([ "\n\n"
                                                         , generator.imports
                                                            |> CodeGen.prettyImports
                                                            |> pretty 1000
                                                         ]
                                                            |> String.concat
                                                        )
                                                   ]
                                                 , case generationModule_.exposing_ of
                                                    Node { end } (Exposing.Explicit _) ->
                                                        [ Fix.insertAt
                                                            { row = end.row, column = end.column - 1 }
                                                            (", " ++ nonExistentFieldLensName)
                                                        ]

                                                    _ ->
                                                        []
                                                 ]
                                                    |> List.concat
                                                )
                                        )
                    )
    in
    Rule.newProjectRuleSchema "NoMissingRecordFieldLens"
        initialProjectContext
        |> Rule.withModuleVisitor
            (\moduleVisitor ->
                moduleVisitor
                    |> Rule.withModuleDefinitionVisitor
                        (\(Node moduleDefinitionRange moduleDefinition) moduleContext ->
                            ( []
                            , case moduleContext of
                                GenerationModule generationModuleContext ->
                                    case moduleDefinition of
                                        NormalModule { exposingList } ->
                                            { generationModuleContext
                                                | exposing_ = exposingList
                                                , beforeDeclarations = moduleDefinitionRange.end
                                            }
                                                |> GenerationModule

                                        _ ->
                                            moduleContext

                                NotGenerationModule _ ->
                                    moduleContext
                            )
                        )
                    |> Rule.withCommentsVisitor
                        (\comments moduleContext ->
                            ( []
                            , case moduleContext of
                                GenerationModule generationModuleContext ->
                                    case
                                        comments
                                            |> List.filter
                                                (String.startsWith "{-|" << Node.value)
                                    of
                                        (Node commentRange _) :: _ ->
                                            { generationModuleContext
                                                | beforeDeclarations = commentRange.end
                                            }
                                                |> GenerationModule

                                        [] ->
                                            moduleContext

                                NotGenerationModule _ ->
                                    moduleContext
                            )
                        )
                    |> Rule.withImportVisitor
                        (\(Node { end } _) moduleContext ->
                            ( []
                            , case moduleContext of
                                GenerationModule generationModuleContext ->
                                    let
                                        before =
                                            generationModuleContext.beforeDeclarations
                                    in
                                    if end.row > before.row then
                                        { generationModuleContext
                                            | beforeDeclarations = end
                                        }
                                            |> GenerationModule

                                    else
                                        moduleContext

                                NotGenerationModule _ ->
                                    moduleContext
                            )
                        )
                    |> Rule.withDeclarationListVisitor
                        (\declarations moduleContext ->
                            ( []
                            , case moduleContext of
                                GenerationModule generationModuleContext ->
                                    { generationModuleContext
                                        | content =
                                            declarations
                                                |> List.filterMap
                                                    (\(Node range declaration) ->
                                                        case declaration of
                                                            FunctionDeclaration fun ->
                                                                ( fun.declaration
                                                                    |> Node.value
                                                                    |> .name
                                                                    |> Node.value
                                                                , range
                                                                )
                                                                    |> Just

                                                            _ ->
                                                                Nothing
                                                    )
                                                |> Dict.fromList
                                    }
                                        |> GenerationModule

                                NotGenerationModule _ ->
                                    moduleContext
                            )
                        )
                    |> Rule.withExpressionEnterVisitor
                        (\(Node expressionRange expression) moduleContext ->
                            ( []
                            , case moduleContext of
                                GenerationModule _ ->
                                    moduleContext

                                NotGenerationModule notGenerationModuleContext ->
                                    (case expression of
                                        Expression.FunctionOrValue _ potentialFieldName ->
                                            case
                                                ModuleNameLookupTable.moduleNameAt
                                                    notGenerationModuleContext.moduleNameLookupTable
                                                    expressionRange
                                            of
                                                Just moduleName ->
                                                    if moduleName == generationModule then
                                                        let
                                                            fieldName =
                                                                potentialFieldName
                                                        in
                                                        { notGenerationModuleContext
                                                            | usages =
                                                                notGenerationModuleContext.usages
                                                                    |> Dict.insert fieldName expressionRange
                                                        }

                                                    else
                                                        notGenerationModuleContext

                                                Nothing ->
                                                    notGenerationModuleContext

                                        _ ->
                                            notGenerationModuleContext
                                    )
                                        |> NotGenerationModule
                            )
                        )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = projectContextToModule
            , fromModuleToProject = moduleContextToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { modulesWithUsages :
        List
            { key : ModuleKey
            , usages : Dict String Range
            }
    , generationModuleInfo :
        Maybe
            { key : ModuleKey
            , moduleNameRange : Range
            , exposing_ : Node Exposing
            , beforeDeclarations : Location
            , content : Dict String Range
            }
    }


type ModuleContext
    = GenerationModule
        { content : Dict String Range
        , beforeDeclarations : Location
        , exposing_ : Node Exposing
        }
    | NotGenerationModule
        { usages : Dict String Range
        , moduleNameLookupTable : ModuleNameLookupTable
        }



--


nonExistentFieldLensNameInfo : String -> { message : String, details : List String }
nonExistentFieldLensNameInfo nonExistentFieldLensName =
    { message =
        "lens for the field `." ++ nonExistentFieldLensName ++ "` doesn't exists yet"
    , details = [ "Add the auto-generated lens through the fix." ]
    }


generationModuleDoesntExistInfo : List String -> { message : String, details : List String }
generationModuleDoesntExistInfo generationModule =
    { message = "record field lenses module doesn't exists yet"
    , details =
        [ [ "Create a module \""
          , generationModule |> String.join "."
          , ".elm\" that will contain all record field lenses."
          ]
            |> String.concat
        , "Field lenses can then be accessed from every other module"
        ]
    }



--


type alias Config =
    { generator : FieldLensGenerator
    , generateIn : ( String, List String )
    }


{-| How to generate a [`FieldLensDeclaration`](NoMissingRecordFieldLens#FieldLensDeclaration) plus the necessary imports.
-}
type alias FieldLensGenerator =
    { imports : List CodeGen.Import
    , declaration :
        { fieldName : String }
        -> FieldLensDeclaration
    }


{-| All the components to build a field lens declaration.
-}
type alias FieldLensDeclaration =
    { documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
    , name : String
    , annotation : Maybe CodeGen.TypeAnnotation
    , implementation : CodeGen.Expression
    }


{-| Print a lens declaration.

    test "custom FieldLensGenerator"
        (\() ->
            customFieldLensGenerator.declaration
                { fieldName = "test" }
                |> printFieldLensDeclaration
                |> Expect.equal
                    """{-| A lens for the field `.test`.
    -}
    test : CustomLens { record | test : test } test
    test =
        customLens
            { access = .test
            , set = \\test_ r -> { r | test = test_ }
            }"""
        )

-}
printFieldLensDeclaration : FieldLensDeclaration -> String
printFieldLensDeclaration =
    \{ documentation, annotation, implementation, name } ->
        CodeGen.funDecl
            documentation
            annotation
            name
            []
            implementation
            |> CodeGen.prettyDeclaration 100
            |> pretty 100
