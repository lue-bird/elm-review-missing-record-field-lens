module NoMissingRecordFieldHelper.Internal exposing (nonExistentFieldHelperNameInfo, printFieldHelperDeclaration, rule)

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
        fieldHelperesModule =
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
                    if moduleName == fieldHelperesModule then
                        FieldHelperesModule
                            { content = Dict.empty
                            , beforeDeclarations = { row = 1, column = 1 }
                            , exposing_ = Node Range.emptyRange (Exposing.Explicit [])
                            }

                    else
                        NotFieldHelperesModule
                            { usedFieldHelperes = Dict.empty
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
                        FieldHelperesModule { content, beforeDeclarations, exposing_ } ->
                            { modulesThatUseFieldHelperes = []
                            , fieldHelperesModuleInfo =
                                Just
                                    { key = moduleKey
                                    , moduleNameRange = moduleNameRange
                                    , exposing_ = exposing_
                                    , beforeDeclarations = beforeDeclarations
                                    , content = content
                                    }
                            }

                        NotFieldHelperesModule { usedFieldHelperes } ->
                            { modulesThatUseFieldHelperes =
                                [ { key = moduleKey
                                  , usedFieldHelperes = usedFieldHelperes
                                  }
                                ]
                            , fieldHelperesModuleInfo = Nothing
                            }
                )
                |> Rule.withMetadata
                |> Rule.withModuleKey

        foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
        foldProjectContexts aContext bContext =
            { modulesThatUseFieldHelperes =
                [ aContext.modulesThatUseFieldHelperes
                , bContext.modulesThatUseFieldHelperes
                ]
                    |> List.concat
            , fieldHelperesModuleInfo =
                [ aContext, bContext ]
                    |> List.map .fieldHelperesModuleInfo
                    |> firstJust
            }

        initialProjectContext : ProjectContext
        initialProjectContext =
            { modulesThatUseFieldHelperes = []
            , fieldHelperesModuleInfo = Nothing
            }

        finalEvaluation : ProjectContext -> List (Rule.Error e_)
        finalEvaluation { modulesThatUseFieldHelperes, fieldHelperesModuleInfo } =
            modulesThatUseFieldHelperes
                |> List.concatMap
                    (\moduleThatUseFieldHelperes ->
                        case fieldHelperesModuleInfo of
                            Just fieldHelperesModule_ ->
                                Dict.diff moduleThatUseFieldHelperes.usedFieldHelperes
                                    fieldHelperesModule_.content
                                    |> Dict.keys
                                    |> List.map
                                        (\nonExistentFieldHelperName ->
                                            Rule.errorForModuleWithFix
                                                fieldHelperesModule_.key
                                                (nonExistentFieldHelperNameInfo nonExistentFieldHelperName)
                                                fieldHelperesModule_.moduleNameRange
                                                (let
                                                    insertLocationInDeclarations =
                                                        fieldHelperesModule_.content
                                                            |> Dict.toList
                                                            |> List.dropWhileRight
                                                                (\( existing, _ ) ->
                                                                    existing > nonExistentFieldHelperName
                                                                )
                                                            |> List.head
                                                 in
                                                 [ [ Fix.insertAt
                                                        (case insertLocationInDeclarations of
                                                            Just ( _, range ) ->
                                                                range.end

                                                            Nothing ->
                                                                fieldHelperesModule_.beforeDeclarations
                                                        )
                                                        (generator
                                                            |> List.concatMap
                                                                (\{ declaration } ->
                                                                    [ "\n\n\n"
                                                                    , printFieldHelperDeclaration
                                                                        { fieldName = nonExistentFieldHelperName }
                                                                        declaration
                                                                    ]
                                                                )
                                                            |> String.concat
                                                        )
                                                   , Fix.insertAt
                                                        (fieldHelperesModule_.exposing_
                                                            |> Node.range
                                                            |> .end
                                                        )
                                                        ([ "\n\n"
                                                         , generator
                                                            |> List.concatMap .imports
                                                            |> CodeGen.prettyImports
                                                            |> pretty 1000
                                                         ]
                                                            |> String.concat
                                                        )
                                                   ]
                                                 , case fieldHelperesModule_.exposing_ of
                                                    Node { end } (Exposing.Explicit _) ->
                                                        [ Fix.insertAt
                                                            { row = end.row, column = end.column - 1 }
                                                            (", " ++ nonExistentFieldHelperName)
                                                        ]

                                                    _ ->
                                                        []
                                                 ]
                                                    |> List.concat
                                                )
                                        )

                            Nothing ->
                                case
                                    moduleThatUseFieldHelperes.usedFieldHelperes
                                        |> Dict.values
                                of
                                    firstRange :: _ ->
                                        [ Rule.errorForModule
                                            moduleThatUseFieldHelperes.key
                                            (fieldHelperesModuleDoesntExistInfo fieldHelperesModule)
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
                        FieldHelperesModule fieldHelperesModuleContext ->
                            case moduleDefinition of
                                NormalModule { exposingList } ->
                                    { fieldHelperesModuleContext
                                        | exposing_ = exposingList
                                        , beforeDeclarations = moduleDefinitionRange.end
                                    }
                                        |> FieldHelperesModule

                                _ ->
                                    moduleContext

                        NotFieldHelperesModule _ ->
                            moduleContext
                    )
                )
                >> Rule.withCommentsVisitor
                    (\comments moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldHelperesModule fieldHelperesModuleContext ->
                                case
                                    comments
                                        |> List.filter
                                            (String.startsWith "{-|" << Node.value)
                                of
                                    (Node commentRange _) :: _ ->
                                        { fieldHelperesModuleContext
                                            | beforeDeclarations = commentRange.end
                                        }
                                            |> FieldHelperesModule

                                    [] ->
                                        moduleContext

                            NotFieldHelperesModule _ ->
                                moduleContext
                        )
                    )
                >> Rule.withImportVisitor
                    (\(Node { end } _) moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldHelperesModule fieldHelperesModuleContext ->
                                let
                                    before =
                                        fieldHelperesModuleContext.beforeDeclarations
                                in
                                if end.row > before.row then
                                    { fieldHelperesModuleContext
                                        | beforeDeclarations = end
                                    }
                                        |> FieldHelperesModule

                                else
                                    moduleContext

                            NotFieldHelperesModule _ ->
                                moduleContext
                        )
                    )
                >> Rule.withDeclarationListVisitor
                    (\declarations moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldHelperesModule fieldHelperesModuleContext ->
                                { fieldHelperesModuleContext
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
                                    |> FieldHelperesModule

                            NotFieldHelperesModule _ ->
                                moduleContext
                        )
                    )
                >> Rule.withExpressionEnterVisitor
                    (\(Node expressionRange expression) moduleContext ->
                        ( []
                        , case moduleContext of
                            FieldHelperesModule _ ->
                                moduleContext

                            NotFieldHelperesModule notFieldHelperesModuleContext ->
                                (case expression of
                                    Expression.FunctionOrValue _ potentialFieldName ->
                                        case
                                            ModuleNameLookupTable.moduleNameAt
                                                notFieldHelperesModuleContext.moduleNameLookupTable
                                                expressionRange
                                        of
                                            Just moduleName ->
                                                if moduleName == fieldHelperesModule then
                                                    let
                                                        fieldName =
                                                            potentialFieldName
                                                    in
                                                    { notFieldHelperesModuleContext
                                                        | usedFieldHelperes =
                                                            notFieldHelperesModuleContext.usedFieldHelperes
                                                                |> Dict.insert fieldName expressionRange
                                                    }

                                                else
                                                    notFieldHelperesModuleContext

                                            Nothing ->
                                                notFieldHelperesModuleContext

                                    _ ->
                                        notFieldHelperesModuleContext
                                )
                                    |> NotFieldHelperesModule
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
    { modulesThatUseFieldHelperes :
        List
            { key : ModuleKey
            , usedFieldHelperes : Dict String Range
            }
    , fieldHelperesModuleInfo :
        Maybe
            { key : ModuleKey
            , moduleNameRange : Range
            , exposing_ : Node Exposing
            , beforeDeclarations : Location
            , content : Dict String Range
            }
    }


type ModuleContext
    = FieldHelperesModule
        { content : Dict String Range
        , beforeDeclarations : Location
        , exposing_ : Node Exposing
        }
    | NotFieldHelperesModule
        { usedFieldHelperes : Dict String Range
        , moduleNameLookupTable : ModuleNameLookupTable
        }



--


nonExistentFieldHelperNameInfo : String -> { message : String, details : List String }
nonExistentFieldHelperNameInfo nonExistentFieldHelperName =
    { message = "No lens for the field `." ++ nonExistentFieldHelperName ++ "` exists yet"
    , details = [ "Add the auto-generated lens through the fix." ]
    }


fieldHelperesModuleDoesntExistInfo : List String -> { message : String, details : List String }
fieldHelperesModuleDoesntExistInfo fieldHelperesModule =
    { message = "No record field lenses module exists yet"
    , details =
        [ [ "Create a module \""
          , fieldHelperesModule |> String.join "."
          , ".elm\" that will contain all record field lenses."
          ]
            |> String.concat
        , "Field lenses can then be accessed from every other module"
        ]
    }



--


type alias Config =
    { generator : List FieldHelperGenerator
    , generateIn : ( String, List String )
    }


{-| How to generate a [`FieldHelperDeclaration`](NoMissingRecordFieldHelper#FieldHelperDeclaration) plus the necessary imports.
-}
type alias FieldHelperGenerator =
    { imports : List CodeGen.Import
    , declaration :
        { fieldName : String }
        -> FieldHelperDeclaration
    }


{-| All the components to build a field lens declaration.
-}
type alias FieldHelperDeclaration =
    { documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
    , name : String
    , annotation : Maybe CodeGen.TypeAnnotation
    , implementation : CodeGen.Expression
    }


{-| Print a lens declaration.

    test "custom FieldHelperGenerator"
        (\() ->
            customFieldHelperGenerator.declaration
                { fieldName = "test" }
                |> printFieldHelperDeclaration
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
printFieldHelperDeclaration : { fieldName : String } -> ({ fieldName : String } -> FieldHelperDeclaration) -> String
printFieldHelperDeclaration { fieldName } fieldHelperGenerator =
    let
        { documentation, annotation, implementation, name } =
            fieldHelperGenerator { fieldName = fieldName }
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
