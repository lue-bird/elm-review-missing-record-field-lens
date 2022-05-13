module NoMissingConstructorPrism exposing (rule)

{-|

@docs rule

-}

import Dict
import Elm.CodeGen as CodeGen exposing (TypeAnnotation)
import Elm.Pretty exposing (prettyDeclaration)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Pretty exposing (pretty)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Generate `elm-accessors` based Prisms for
custom Types that don't already have one defined.

Because functions in elm can't start with a capitol letter OR an underscore `_`
The prefix `c_` is used as a way to namespace the generated code.

    config =
        [ NoMissingConstructorPrism.rule
        ]

see the `tests/` for examples of the sort of code that's generated.


## use it

... when you're using `elm-accessors` to mitigate
boilerplate related to updating potentially deeply nested data.


## don't use it

... when you consider lenses the less readable/intuitive/simple/explicit alternative.


## try it without installation

```bash
elm-review --rules NoMissingConstructorPrism SomeModule.elm
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingConstructorPrism" ()
        -- Add your visitors
        -- |> Rule.fromModuleRuleSchema
        |> Rule.withDeclarationListVisitor generatePrismFromTypeDecl
        |> Rule.fromModuleRuleSchema


generatePrismFromTypeDecl : List (Node Declaration) -> moduleContext -> ( List (Error {}), moduleContext )
generatePrismFromTypeDecl nodes ctx =
    ( List.foldr
        (\(Node injectionRange node) acc ->
            case node of
                FunctionDeclaration fn ->
                    let
                        fnName : String
                        fnName =
                            fn.declaration
                                |> Node.value
                                |> .name
                                |> Node.value

                        ctorName : String
                        ctorName =
                            fnName |> String.dropLeft 2
                    in
                    if fnName |> String.startsWith "c_" then
                        Dict.insert ctorName Nothing acc

                    else
                        acc

                CustomTypeDeclaration type_ ->
                    let
                        isAdt : Bool
                        isAdt =
                            -- If custom type only has one constructor then
                            -- we can generate a Lens.
                            List.length type_.constructors > 1
                    in
                    if isAdt then
                        List.foldr
                            (\(Node errorRange ctor) ->
                                let
                                    ctorName : String
                                    ctorName =
                                        Node.value ctor.name
                                in
                                if List.length ctor.arguments > 0 then
                                    Dict.update ctorName
                                        (\m ->
                                            case m of
                                                Nothing ->
                                                    Just
                                                        (Just
                                                            (Rule.errorWithFix
                                                                { message = "Generating a `c_" ++ ctorName ++ "` Prism for the type constructor: `" ++ ctorName ++ "`."
                                                                , details = [ "missing prism for constructor `" ++ ctorName ++ "`" ]
                                                                }
                                                                errorRange
                                                                [ Fix.insertAt
                                                                    { row = injectionRange.end.row + 1
                                                                    , column = injectionRange.end.column
                                                                    }
                                                                    ("\n\n\n"
                                                                        ++ printPrism type_ ctor
                                                                    )
                                                                ]
                                                            )
                                                        )

                                                otherwise ->
                                                    otherwise
                                        )

                                else
                                    identity
                            )
                            acc
                            type_.constructors

                    else
                        acc

                _ ->
                    acc
        )
        Dict.empty
        nodes
        |> Dict.values
        |> List.filterMap identity
    , ctx
    )


printPrism : Type -> ValueConstructor -> String
printPrism t ctor =
    let
        typeName : String
        typeName =
            Node.value t.name

        generics : List String
        generics =
            List.map Node.value t.generics

        ctorName : String
        ctorName =
            Node.value ctor.name

        { access, update } =
            implementation ctor

        fnName : String
        fnName =
            "c_" ++ ctorName
    in
    CodeGen.funDecl
        Nothing
        -- TODO: Upgrade once I figure out a `type alias Optional`
        (CodeGen.funAnn
            (CodeGen.typed "Relation"
                ((ctor.arguments
                    |> List.map Node.value
                    |> nested CodeGen.tupleAnn
                 )
                    ++ [ CodeGen.typeVar "reachable"
                       , CodeGen.typeVar "wrap"
                       ]
                )
            )
            (CodeGen.typed "Relation"
                [ CodeGen.typed typeName (List.map CodeGen.typeVar generics)
                , CodeGen.typeVar "reachable"
                , CodeGen.maybeAnn (CodeGen.typeVar "wrap")
                ]
            )
            |> Just
        )
        fnName
        []
        -- TODO: Add `import Accessors exposing (makeOneToN_)` or whatever for any
        -- modules we're generating code for.
        (CodeGen.construct "makeOneToN_"
            [ CodeGen.string fnName
            , access
            , update
            ]
        )
        |> prettyDeclaration 100
        |> pretty 100


implementation :
    ValueConstructor
    ->
        { access : CodeGen.Expression

        -- , set : CodeGen.Expression
        , update : CodeGen.Expression
        }
implementation ctor =
    let
        ctorName : String
        ctorName =
            Node.value ctor.name
    in
    { access =
        CodeGen.lambda
            [ CodeGen.varPattern "fn"
            , CodeGen.varPattern "t"
            ]
            (CodeGen.caseExpr (CodeGen.val "t")
                [ ( CodeGen.namedPattern ctorName
                        (List.indexedMap (\ix _ -> CodeGen.varPattern ("a" ++ String.fromInt ix)) ctor.arguments)
                  , CodeGen.construct "Just"
                        [ CodeGen.parens
                            (CodeGen.construct "fn"
                                --[ CodeGen.tuple (List.indexedMap (\ix _ -> CodeGen.val ("a" ++ String.fromInt ix)) ctor.arguments)
                                (ctor.arguments
                                    |> List.indexedMap (\ix _ -> CodeGen.val ("a" ++ String.fromInt ix))
                                    |> nested CodeGen.tuple
                                )
                            )
                        ]
                  )
                , ( CodeGen.allPattern, CodeGen.val "Nothing" )
                ]
            )
    , update =
        CodeGen.lambda
            [ CodeGen.varPattern "fn"
            , CodeGen.varPattern "t"
            ]
            (CodeGen.caseExpr (CodeGen.val "t")
                [ ( CodeGen.namedPattern ctorName
                        (List.indexedMap (\ix _ -> CodeGen.varPattern ("a" ++ String.fromInt ix)) ctor.arguments)
                  , if List.length ctor.arguments > 1 then
                        CodeGen.letExpr
                            [ CodeGen.letFunction "apply_c"
                                (CodeGen.varPattern "ctor"
                                    :: (ctor.arguments
                                            |> List.indexedMap (\ix _ -> CodeGen.varPattern ("t" ++ String.fromInt ix))
                                            |> nested CodeGen.tuplePattern
                                       )
                                )
                                (CodeGen.construct "ctor" (ctor.arguments |> List.indexedMap (\ix _ -> CodeGen.val ("t" ++ String.fromInt ix))))
                            ]
                            (CodeGen.construct "apply_c"
                                (CodeGen.fun ctorName
                                    :: [ CodeGen.parens
                                            (CodeGen.construct "fn"
                                                --[ CodeGen.tuple (List.indexedMap (\ix _ -> CodeGen.val ("a" ++ String.fromInt ix)) ctor.arguments)
                                                (ctor.arguments
                                                    |> List.indexedMap (\ix _ -> CodeGen.val ("a" ++ String.fromInt ix))
                                                    |> nested CodeGen.tuple
                                                )
                                            )
                                       ]
                                )
                            )

                    else
                        CodeGen.construct ctorName [ CodeGen.parens (CodeGen.construct "fn" [ CodeGen.val "a0" ]) ]
                  )
                , ( CodeGen.namedPattern "otherwise" [], CodeGen.val "otherwise" )
                ]
            )
    }


nested : (List a -> a) -> List a -> List a
nested toA tpls =
    case tpls of
        one :: two :: rest ->
            [ toA (one :: nested toA (two :: rest)) ]

        otherwise ->
            otherwise
