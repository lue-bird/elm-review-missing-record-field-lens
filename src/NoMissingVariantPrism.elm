module NoMissingVariantPrism exposing
    ( rule
    , accessors
    , VariantPrismGenerator, VariantPrismDeclaration, implementation, withDocumentation, withName
    )

{-|

@docs rule


# lens generators


## working out of the box

@docs accessors


## custom

@docs VariantPrismGenerator, VariantPrismDeclaration, implementation, withDocumentation, withName

-}

import Dict exposing (Dict)
import Elm.CodeGen as CodeGen
import Elm.Pretty as CodeGen
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Hand exposing (Empty, Hand(..))
import Help exposing (indexed)
import Parser exposing ((|.), (|=), Parser)
import Possibly exposing (Possibly(..))
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import Stack exposing (StackTopBelow(..), Stacked)


{-| Generate `elm-accessors` based Prisms for
custom Types that don't already have one defined.

Because functions in elm can't start with a capitol letter OR an underscore `_`
The prefix `variant` is used as a way to namespace the generated code.

    config =
        [ NoMissingVariantPrism.rule
        ]

see the `tests/` for examples of the sort of code that's generated.


## use it

... when you're using `elm-accessors` to mitigate
boilerplate related to updating potentially deeply nested data.


## don't use it

... when you consider lenses the less readable/intuitive/simple/explicit alternative.

-}
rule : { generator : VariantPrismGenerator } -> Rule
rule { generator } =
    Rule.newModuleRuleSchema "NoMissingVariantPrism"
        initialContext
        -- TODO: `import Accessors exposing (makeOneToN_)` in modules we're generating code for
        |> Rule.withDeclarationEnterVisitor
            (\declarationNode context ->
                ( []
                , context |> visitDeclaration declarationNode
                )
            )
        |> Rule.withFinalModuleEvaluation
            (\context ->
                generatePrisms { context = context, generator = generator }
            )
        |> Rule.fromModuleRuleSchema


type alias Context =
    Dict String VariantTypePrism


type VariantTypePrism
    = VariantTypePrismAlreadyExists
    | VariantTypePrismMissing
        { typeRange : Range
        , variantValues : List CodeGen.TypeAnnotation
        , type_ : CodeGen.TypeAnnotation
        , typeDeclarationEnd : Location
        }


initialContext : Context
initialContext =
    Dict.empty


variantPrismNameParser : Parser String
variantPrismNameParser =
    Parser.succeed identity
        |. Parser.token "variant"
        |= (Parser.succeed () |> Parser.getChompedString)


visitDeclaration : Node Declaration -> Context -> Context
visitDeclaration declarationNode =
    let
        (Node declarationRange declaration) =
            declarationNode
    in
    case declaration of
        Declaration.FunctionDeclaration functionDeclaration ->
            let
                functionName : String
                functionName =
                    functionDeclaration.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            case functionName |> Parser.run variantPrismNameParser of
                Ok prismName ->
                    Dict.insert prismName VariantTypePrismAlreadyExists

                Err _ ->
                    identity

        Declaration.CustomTypeDeclaration type_ ->
            case type_.constructors of
                -- If it only has one variant then we can't generate a lens (is that what you wanted to comment @erlandsona ?)
                _ :: _ :: _ ->
                    \context ->
                        type_.constructors
                            |> List.foldr
                                (\(Node typeRange variant) ->
                                    case variant.arguments of
                                        [] ->
                                            identity

                                        _ :: _ ->
                                            Dict.update (variant.name |> Node.value)
                                                (\m ->
                                                    case m of
                                                        Nothing ->
                                                            VariantTypePrismMissing
                                                                { typeRange = typeRange
                                                                , type_ =
                                                                    CodeGen.typed
                                                                        (type_.name |> Node.value)
                                                                        (type_.generics
                                                                            |> List.map
                                                                                (\(Node _ typeVarName) ->
                                                                                    CodeGen.typeVar typeVarName
                                                                                )
                                                                        )
                                                                , typeDeclarationEnd = declarationRange.end
                                                                , variantValues =
                                                                    variant.arguments
                                                                        |> List.map Node.value
                                                                }
                                                                |> Just

                                                        Just variantTypePrismInfo ->
                                                            variantTypePrismInfo |> Just
                                                )
                                )
                                context

                [ _ ] ->
                    identity

                [] ->
                    identity

        _ ->
            identity


generatePrisms :
    { generator : VariantPrismGenerator, context : Context }
    -> List (Rule.Error {})
generatePrisms { context, generator } =
    context
        |> Dict.toList
        |> List.filterMap
            (\( variantName, variantTypePrismInfo ) ->
                case variantTypePrismInfo of
                    VariantTypePrismAlreadyExists ->
                        Nothing

                    VariantTypePrismMissing variantTypePrismInfoForGenerating ->
                        ( variantName, variantTypePrismInfoForGenerating ) |> Just
            )
        |> List.map
            (\( variantName, { variantValues, type_, typeDeclarationEnd, typeRange } ) ->
                Rule.errorWithFix
                    { message = "The variant `" ++ variantName ++ "` doesn't have a prism."
                    , details = [ "Add the auto-generated lens through the fix." ]
                    }
                    typeRange
                    [ [ "\n\n\n"
                      , generator.declaration
                            { type_ = type_
                            , variantName = variantName
                            , variantValues = variantValues
                            }
                            |> prismDeclarationCodeGen
                            |> CodeGen.prettyDeclaration 100
                            |> pretty 100
                      ]
                        |> String.concat
                        |> Fix.insertAt
                            { typeDeclarationEnd
                                | row = typeDeclarationEnd.row + 1
                            }
                    ]
            )


prismDeclarationCodeGen : VariantPrismDeclaration -> CodeGen.Declaration
prismDeclarationCodeGen =
    \prismDeclaration ->
        CodeGen.funDecl
            prismDeclaration.documentation
            prismDeclaration.annotation
            prismDeclaration.name
            []
            prismDeclaration.implementation


implementation :
    { variantName : String
    , variantValues : List CodeGen.TypeAnnotation
    }
    ->
        { access : CodeGen.Expression
        , alter : CodeGen.Expression
        }
implementation { variantName, variantValues } =
    let
        variantValuesCode =
            case variantValues |> Stack.fromList of
                Empty Possible ->
                    { expression = CodeGen.unit
                    , pattern = CodeGen.unitPattern
                    }

                Filled topDown ->
                    { expression =
                        Filled topDown
                            |> Stack.map
                                (\{ index } _ ->
                                    CodeGen.val ("value" |> indexed index)
                                )
                            |> Stack.fold (\value soFar -> CodeGen.tuple [ soFar, value ])
                    , pattern =
                        Filled topDown
                            |> Stack.map
                                (\{ index } _ ->
                                    CodeGen.varPattern ("value" |> indexed index)
                                )
                            |> Stack.fold (\value soFar -> CodeGen.tuplePattern [ soFar, value ])
                    }
    in
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
                        variantValuesCode.expression
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
                [ ( CodeGen.namedPattern variantName
                        (variantValues
                            |> List.indexedMap
                                (\index _ ->
                                    CodeGen.varPattern ("value" |> indexed index)
                                )
                        )
                  , case variantValues of
                        _ :: (_ :: _) ->
                            CodeGen.letExpr
                                [ CodeGen.letFunction "variantValuesCurryTo"
                                    [ CodeGen.varPattern "variantTagValue"
                                    , variantValuesCode.pattern
                                    ]
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
                                    variantValuesCode.expression
                                    CodeGen.piper
                                    [ CodeGen.fun "variantValuesAlter"
                                    , CodeGen.construct "variantValuesCurryTo" [ CodeGen.fun variantName ]
                                    ]
                                )

                        _ ->
                            CodeGen.construct variantName
                                [ CodeGen.construct "variantValuesAlter"
                                    [ CodeGen.val ("value" |> indexed 0) ]
                                    |> CodeGen.parens
                                ]
                  )
                , ( CodeGen.namedPattern "notVariant" []
                  , CodeGen.val "notVariant"
                  )
                ]
            )
    }


accessors : VariantPrismGenerator
accessors =
    { imports = []
    , declaration =
        \{ variantName, type_, variantValues } ->
            let
                functionName =
                    "variant" ++ variantName
            in
            { documentation =
                CodeGen.emptyDocComment
                    |> CodeGen.markdown
                        ([ "Accessor prism for the variant `"
                         , variantName
                         , "` of the `type` `"
                         , type_ |> CodeGen.prettyTypeAnnotation |> pretty 100
                         , "`."
                         ]
                            |> String.concat
                        )
                    |> Just
            , name = functionName
            , annotation =
                -- TODO: upgrade once @erlandsona figures out a `type alias Optional`
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
                        [ type_
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
                    [ CodeGen.string functionName
                    , access
                    , alter
                    ]
            }
    }


{-| How to generate a [`VariantPrismDeclaration`](#VariantPrismDeclaration) plus the necessary `import`s.

Out of the box there are lenses for

  - [`elm-accessors`](#accessors)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    customLens : VariantPrismGenerator
    customLens =
        { imports =
            [ importStmt [ "CustomLens" ]
                Nothing
                (exposeExplicit
                    [ typeOrAliasExpose "CustomLens" ]
                    |> Just
                )
            ]
        , declaration =
            \{ variantName } ->
                { documentation =
                    emptyDocComment
                        |> markdown
                            ("`CustomLens` for the field `." ++ variantName ++ "`.")
                        |> Just
                , name = variantName
                , annotation =
                    typed "CustomLens"
                        [ extRecordAnn "record"
                            [ ( variantName, typeVar variantName ) ]
                        , typeVar variantName
                        ]
                        |> Just
                , implementation =
                    let
                        { access, set } =
                            functionsForField variantName
                    in
                    fqConstruct [ "CustomLens" ] "create" [ access, at ]
                }
        }

-}
type alias VariantPrismGenerator =
    { imports : List CodeGen.Import
    , declaration :
        { type_ : CodeGen.TypeAnnotation
        , variantName : String
        , variantValues : List CodeGen.TypeAnnotation
        }
        -> VariantPrismDeclaration
    }


{-| All the components to build a field lens declaration:

    {-| [documentation]
    -}
    [name] : [annotation]
    [name] =
        [implementation]

You can customize existing `VariantPrismDeclaration`s with [`withDocumentation`](#withDocumentation) and [`withName`](#withName)
or create custom lens ([`functionsForField`](#functionsForField) and [`getSetRecordForField`](#getSetRecordForField) can be helpful).

    customLensDeclaration { variantName } =
        { documentation =
            emptyDocComment
                |> markdown
                    ("`CustomLens` for the field `." ++ variantName ++ "`.")
                |> Just
        , name = variantName
        , annotation =
            typed "CustomLens"
                [ extRecordAnn "record"
                    [ ( variantName, typeVar variantName ) ]
                , typeVar variantName
                ]
                |> Just
        , implementation =
            let
                { access, set } =
                    functionsForField variantName
            in
            fqConstruct [ "CustomLens" ] "create" [ access, at ]
        }

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


{-| Use a different name for the generated lens.

    accessorsWithPrefix_v_ { variantName } =
        accessors { variantName = variantName }
            |> withName ("v_" ++ variantName)

-}
withName :
    String
    -> VariantPrismDeclaration
    -> VariantPrismDeclaration
withName name fieldHelperDeclaration =
    { fieldHelperDeclaration | name = name }
