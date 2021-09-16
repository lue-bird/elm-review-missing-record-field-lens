module NoMissingRecordFieldHelper.Internal exposing (nonExistentFieldLensNameInfo, printFieldLensDeclaration, rule)

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
rule { generator, generateIn } =
    let
        fieldLensesModule =
            let
                ( head, tail ) =
                    generateIn
            in
            head :: tail

        initializeModuleFromProjectContext : ContextCreator ProjectContext ModuleContext
        initializeModuleFromProjectContext =
            Rule.initContextCreator
                (\metadata moduleNameLookupTable _ ->
                    let
                        moduleName =
                            Rule.moduleNameFromMetadata metadata
                    in
                    if moduleName == fieldLensesModule then
                        FieldLensesModule
                            { content = Dict.empty
                            , beforeDeclarations = { row = 1, column = 1 }
                            , exposing_ = Node Range.emptyRange (Exposing.Explicit [])
                            }

                    else
                        NotFieldLensesModule
                            { usedFieldLenses = Dict.empty
                            , moduleNameLookupTable = moduleNameLookupTable
                            }
                )
                |> Rule.withMetadata
                |> Rule.withModuleNameLookupTable

        moduleToProjectContext : ContextCreator ModuleContext ProjectContext
        moduleToProjectContext =
            Rule.initContextCreator
                (\metadata moduleKey moduleContext ->
                    let
                        (Node moduleNameRange _) =
                            Rule.moduleNameNodeFromMetadata metadata
                    in
                    case moduleContext of
                        FieldLensesModule { content, beforeDeclarations, exposing_ } ->
                            { modulesThatUseFieldLenses = []
                            , fieldLensesModuleInfo =
                                Just
                                    { key = moduleKey
                                    , moduleNameRange = moduleNameRange
                                    , exposing_ = exposing_
                                    , beforeDeclarations = beforeDeclarations
                                    , content = content
                                    }
                            }

                        NotFieldLensesModule { usedFieldLenses } ->
                            { modulesThatUseFieldLenses =
                                [ { key = moduleKey
                                  , usedFieldLenses = usedFieldLenses
                                  }
                                ]
                            , fieldLensesModuleInfo = Nothing
                            }
                )
                |> Rule.withMetadata
                |> Rule.withModuleKey

        foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
        foldProjectContexts aContext bContext =
            { modulesThatUseFieldLenses =
                [ aContext.modulesThatUseFieldLenses
                , bContext.modulesThatUseFieldLenses
                ]
                    |> List.concat
            , fieldLensesModuleInfo =
                [ aContext, bContext ]
                    |> List.map .fieldLensesModuleInfo
                    |> firstJust
            }

        initialProjectContext : ProjectContext
        initialProjectContext =
            { modulesThatUseFieldLenses = []
            , fieldLensesModuleInfo = Nothing
            }

        finalEvaluation : ProjectContext -> List (Rule.Error e_)
        finalEvaluation { modulesThatUseFieldLenses, fieldLensesModuleInfo } =
            modulesThatUseFieldLenses
                |> List.concatMap
                    (\moduleThatUseFieldLenses ->
                        case fieldLensesModuleInfo of
                            Just fieldLensesModule_ ->
                                Dict.diff moduleThatUseFieldLenses.usedFieldLenses
                                    fieldLensesModule_.content
                                    |> Dict.keys
                                    |> List.map
                                        (\nonExistentFieldLensName ->
                                            Rule.errorForModuleWithFix
                                                fieldLensesModule_.key
                                                (nonExistentFieldLensNameInfo nonExistentFieldLensName)
                                                fieldLensesModule_.moduleNameRange
                                                (let
                                                    insertLocationInDeclarations =
                                                        fieldLensesModule_.content
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
                                                                fieldLensesModule_.beforeDeclarations
                                                        )
                                                        ([ "\n\n\n"
                                                         , printFieldLensDeclaration
                                                            { fieldName = nonExistentFieldLensName }
                                                            generator.declaration
                                                         ]
                                                            |> String.concat
                                                        )
                                                   , Fix.insertAt
                                                        (fieldLensesModule_.exposing_
                                                            |> Node.range
                                                            |> .end
                                                        )
                                                        ([ "\n\n"
                                                         , generator.imports
                                                            |> CodeGen.prettyImports
                                                            |> pretty 1000
                                                         ]
                                                            |> String.concat
                                                        )
                                                   ]
                                                 , case fieldLensesModule_.exposing_ of
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

                            Nothing ->
                                case
                                    moduleThatUseFieldLenses.usedFieldLenses
                                        |> Dict.values
                                of
                                    firstRange :: _ ->
                                        [ Rule.errorForModule
                                            moduleThatUseFieldLenses.key
                                            (fieldLensesModuleDoesntExistInfo fieldLensesModule)
                                            firstRange
                                        ]

                                    [] ->
                                        []
                    )
    in
    Rule.newProjectRuleSchema "NoMissingRecordFieldHelper"
        initialProjectContext
        |> Rule.withModuleVisitor
            (Rule.withModuleDefinitionVisitor
                (\(Node moduleDefinitionRange moduleDefinition) moduleContext ->
                    ( []
                    , case moduleContext of
                        FieldLensesModule fieldLensesModuleContext ->
                            case moduleDefinition of
                                NormalModule { exposingList } ->
                                    { fieldLensesModuleContext
                                        | exposing_ = exposingList
                                        , beforeDeclarations = moduleDefinitionRange.end
                                    }
                                        |> FieldLensesModule

                                _ ->
                                    moduleContext

                        NotFieldLensesModule _ ->
                            moduleContext
                    )
                )
                >> Rule.withCommentsVisitor
                    (\comments moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldLensesModule fieldLensesModuleContext ->
                                case
                                    comments
                                        |> List.filter
                                            (String.startsWith "{-|" << Node.value)
                                of
                                    (Node commentRange _) :: _ ->
                                        { fieldLensesModuleContext
                                            | beforeDeclarations = commentRange.end
                                        }
                                            |> FieldLensesModule

                                    [] ->
                                        moduleContext

                            NotFieldLensesModule _ ->
                                moduleContext
                        )
                    )
                >> Rule.withImportVisitor
                    (\(Node { end } _) moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldLensesModule fieldLensesModuleContext ->
                                let
                                    before =
                                        fieldLensesModuleContext.beforeDeclarations
                                in
                                if end.row > before.row then
                                    { fieldLensesModuleContext
                                        | beforeDeclarations = end
                                    }
                                        |> FieldLensesModule

                                else
                                    moduleContext

                            NotFieldLensesModule _ ->
                                moduleContext
                        )
                    )
                >> Rule.withDeclarationListVisitor
                    (\declarations moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldLensesModule fieldLensesModuleContext ->
                                { fieldLensesModuleContext
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
                                    |> FieldLensesModule

                            NotFieldLensesModule _ ->
                                moduleContext
                        )
                    )
                >> Rule.withExpressionEnterVisitor
                    (\(Node expressionRange expression) moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldLensesModule _ ->
                                moduleContext

                            NotFieldLensesModule notFieldLensesModuleContext ->
                                (case expression of
                                    Expression.FunctionOrValue _ potentialFieldName ->
                                        case
                                            ModuleNameLookupTable.moduleNameAt
                                                notFieldLensesModuleContext.moduleNameLookupTable
                                                expressionRange
                                        of
                                            Just moduleName ->
                                                if moduleName == fieldLensesModule then
                                                    let
                                                        fieldName =
                                                            potentialFieldName
                                                    in
                                                    { notFieldLensesModuleContext
                                                        | usedFieldLenses =
                                                            notFieldLensesModuleContext.usedFieldLenses
                                                                |> Dict.insert fieldName expressionRange
                                                    }

                                                else
                                                    notFieldLensesModuleContext

                                            Nothing ->
                                                notFieldLensesModuleContext

                                    _ ->
                                        notFieldLensesModuleContext
                                )
                                    |> NotFieldLensesModule
                        )
                    )
            )
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = initializeModuleFromProjectContext
            , fromModuleToProject = moduleToProjectContext
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


type alias ProjectContext =
    { modulesThatUseFieldLenses :
        List
            { key : ModuleKey
            , usedFieldLenses : Dict String Range
            }
    , fieldLensesModuleInfo :
        Maybe
            { key : ModuleKey
            , moduleNameRange : Range
            , exposing_ : Node Exposing
            , beforeDeclarations : Location
            , content : Dict String Range
            }
    }


type ModuleContext
    = FieldLensesModule
        { content : Dict String Range
        , beforeDeclarations : Location
        , exposing_ : Node Exposing
        }
    | NotFieldLensesModule
        { usedFieldLenses : Dict String Range
        , moduleNameLookupTable : ModuleNameLookupTable
        }



--


nonExistentFieldLensNameInfo : String -> { message : String, details : List String }
nonExistentFieldLensNameInfo nonExistentFieldLensName =
    { message = "No lens for the field `." ++ nonExistentFieldLensName ++ "` exists yet"
    , details = [ "Add the auto-generated lens through the fix." ]
    }


fieldLensesModuleDoesntExistInfo : List String -> { message : String, details : List String }
fieldLensesModuleDoesntExistInfo fieldLensesModule =
    { message = "No record field lenses module exists yet"
    , details =
        [ [ "Create a module \""
          , fieldLensesModule |> String.join "."
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


{-| How to generate a [`FieldLensDeclaration`](NoMissingRecordFieldHelper#FieldLensDeclaration) plus the necessary imports.
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
printFieldLensDeclaration : { fieldName : String } -> ({ fieldName : String } -> FieldLensDeclaration) -> String
printFieldLensDeclaration { fieldName } fieldLensGenerator =
    let
        { documentation, annotation, implementation, name } =
            fieldLensGenerator { fieldName = fieldName }
    in
    CodeGen.funDecl
        documentation
        annotation
        name
        []
        implementation
        |> CodeGen.prettyDeclaration 100
        |> pretty 100



-- utils


firstJust : List (Maybe a) -> Maybe a
firstJust =
    List.filterMap identity
        >> List.head
