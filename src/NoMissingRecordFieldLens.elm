module NoMissingRecordFieldLens exposing
    ( rule, Config
    , accessors, monocle, fields, zipper, accessorsBChiquet
    , FieldLensGenerator, FieldLensDeclaration, functionsForField, getSetRecordForField, withDocumentation, withName
    )

{-|

@docs rule, Config


# lens generators


## working out of the box

@docs accessors, monocle, fields, zipper, accessorsBChiquet


## custom

@docs FieldLensGenerator, FieldLensDeclaration, functionsForField, getSetRecordForField, withDocumentation, withName

-}

import Elm.CodeGen as CodeGen
import Help exposing (typeLens, typeRelation)
import NoMissingRecordFieldLens.Internal as Internal
import Review.Rule exposing (Rule)


{-| Automatically generates used record field helpers that don't exist yet.

Examples are

  - [`erlandsona/elm-accessors`](https://package.elm-lang.org/packages/erlandsona/elm-accessors/latest/)
  - [`sjorn3/elm-fields`](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/)
  - [`arturopala/elm-monocle`](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest)
  - [`zh5/zipper`](https://package.elm-lang.org/packages/z5h/zipper/latest/)
  - [`bChiquet/elm-accessors`](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)

```
config =
    [ NoMissingRecordFieldLens.rule
        { generator = NoMissingRecordFieldLens.accessors
        , generateIn = ( "Accessors", [ "Library", "Fields" ] )
        }
    ]
```

  - `generator`: What kind of lens to generate:
      - [`elm-accessors`](#accessors),
      - [`elm-fields`](#fields),
      - [`elm-monocle`](#monocle),
      - [`zipper`](#zipper) or
      - [a custom one](#FieldLensGenerator).

  - `generateIn`: The module where all field lenses will be generated in

    understand `( "Accessors", [ "Library", "Fields" ] )` as `Accessors.Library.Fields`


## Example

    module SomeModule exposing (scoreAPoint)

    import Accessors.Library.Fields as Field

    scoreAPoint =
        Accessors.over Field.score (\score -> score + 1)


### Fail

    module Accessors.Library.Fields exposing (name)

    ...


### Success

    module Accessors.Library.Fields exposing (score)

    ...

-}
rule : Config -> Rule
rule config =
    Internal.rule config


{-| The [rule](#rule)'s configuration.

  - `generate`: What kind of lens to generate:
      - [`elm-accessors`](#accessors),
      - [`elm-fields`](#fields),
      - [`elm-monocle`](#monocle),
      - [`zipper`](#zipper) or
      - [`updateField`](#update), [`setField`](#set)
      - [a custom one](#FieldLensGenerator).

  - `generateIn`: The module where all field lenses will be generated in

    read `( "Module", [ "Name" ] )` as `Module.Name`

-}
type alias Config =
    { generator : FieldLensGenerator
    , generateIn : ( String, List String )
    }


{-| How to generate a [`FieldLensDeclaration`](#FieldLensDeclaration) plus the necessary imports.

Out of the box there are lenses for

  - [`erlandsona/elm-accessors`](#accessors)
  - [`elm-fields`](#fields)
  - [`elm-monocle`](#monocle)
  - [`zipper`](#zipper)
  - [`bChiquet/elm-accessors`](#accessorsBChiquet)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    customLens : FieldLensGenerator
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
            \{ fieldName } ->
                { documentation =
                    emptyDocComment
                        |> markdown
                            ("`CustomLens` for the field `." ++ fieldName ++ "`.")
                        |> Just
                , name = fieldName
                , annotation =
                    typed "CustomLens"
                        [ extRecordAnn "record"
                            [ ( fieldName, typeVar fieldName ) ]
                        , typeVar fieldName
                        ]
                        |> Just
                , implementation =
                    let
                        { access, set } =
                            functionsForField fieldName
                    in
                    fqConstruct [ "CustomLens" ] "create" [ access, at ]
                }
        }

-}
type alias FieldLensGenerator =
    { imports : List CodeGen.Import
    , declaration :
        { fieldName : String }
        -> FieldLensDeclaration
    }


{-| All the components to build a field lens declaration:

    {-| [documentation]
    -}
    [name] : [annotation]
    [name] =
        [implementation]

You can customize existing `FieldLensDeclaration`s with [`withDocumentation`](#withDocumentation) and [`withName`](#withName)
or create custom lens ([`functionsForField`](#functionsForField) and [`getSetRecordForField`](#getSetRecordForField) can be helpful).

    customLensDeclaration { fieldName } =
        { documentation =
            emptyDocComment
                |> markdown
                    ("`CustomLens` for the field `." ++ fieldName ++ "`.")
                |> Just
        , name = fieldName
        , annotation =
            typed "CustomLens"
                [ extRecordAnn "record"
                    [ ( fieldName, typeVar fieldName ) ]
                , typeVar fieldName
                ]
                |> Just
        , implementation =
            let
                { access, set } =
                    functionsForField fieldName
            in
            fqConstruct [ "CustomLens" ] "create" [ access, at ]
        }

-}
type alias FieldLensDeclaration =
    { documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
    , name : String
    , annotation : Maybe CodeGen.TypeAnnotation
    , implementation : CodeGen.Expression
    }


{-| [`FieldLensGenerator`](#FieldLensGenerator)
for named [`erlandsona/elm-accessors`](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/)
in the form

    import Accessors exposing (Lens, makeOneToOne_)

    score : Lens { record | score : score } transformed score wrap
    score =
        makeOneToOne_ ".score" .score (\alter record -> { record | score = record.score |> alter })

-}
accessors : FieldLensGenerator
accessors =
    { imports =
        [ CodeGen.importStmt [ "Accessors" ]
            Nothing
            (CodeGen.exposeExplicit
                [ CodeGen.funExpose "makeOneToOne_"
                , CodeGen.typeOrAliasExpose "Lens"
                ]
                |> Just
            )
        ]
    , declaration =
        \{ fieldName } ->
            { documentation = Nothing
            , name = fieldName
            , annotation =
                typeLens
                    (CodeGen.extRecordAnn "record"
                        [ ( fieldName, CodeGen.typeVar fieldName ) ]
                    )
                    (CodeGen.typeVar "transformed")
                    (CodeGen.typeVar fieldName)
                    (CodeGen.typeVar "wrap")
                    |> Just
            , implementation =
                let
                    { access, update } =
                        functionsForField fieldName
                in
                CodeGen.construct "makeOneToOne_"
                    [ CodeGen.string (String.cons '.' fieldName)
                    , access
                    , update
                    ]
            }
    }


{-| [`FieldLensGenerator`](#FieldLensGenerator) for [`bChiquet/elm-accessors`](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest) in the form

    import Accessors exposing (Relation, makeOneToOne)

    score : Relation score transformed wrap -> Relation { record | score : score } transformed wrap
    score =
        makeOneToOne .score (\alter record -> { record | score = record.score |> alter })

[`accessors`](#accessors) generates for [`erlandsona/elm-accessors`](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/) which adds names.

-}
accessorsBChiquet : FieldLensGenerator
accessorsBChiquet =
    { imports =
        [ CodeGen.importStmt [ "Accessors" ]
            Nothing
            (CodeGen.exposeExplicit
                [ CodeGen.funExpose "makeOneToOne"
                , CodeGen.typeOrAliasExpose "Relation"
                ]
                |> Just
            )
        ]
    , declaration =
        \{ fieldName } ->
            { documentation = Nothing
            , name = fieldName
            , annotation =
                let
                    relation super =
                        typeRelation
                            super
                            (CodeGen.typeVar "transformed")
                            (CodeGen.typeVar "wrap")
                in
                CodeGen.funAnn
                    (relation (CodeGen.typeVar fieldName))
                    (relation
                        (CodeGen.extRecordAnn "record"
                            [ ( fieldName, CodeGen.typeVar fieldName ) ]
                        )
                    )
                    |> Just
            , implementation =
                let
                    { access, update } =
                        functionsForField fieldName
                in
                CodeGen.construct "makeOneToOne" [ access, update ]
            }
    }


{-| [`FieldLensGenerator`](#FieldLensGenerator) for [arturopala's elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest) in the form

    import Monocle.Lens exposing (Lens)

    score : Lens { record | score : score } score
    score =
        { get = .score, set = \replacement record -> { record | score = replacement } }

-}
monocle : FieldLensGenerator
monocle =
    { imports =
        [ CodeGen.importStmt [ "Monocle", "Lens" ]
            Nothing
            (CodeGen.exposeExplicit
                [ CodeGen.typeOrAliasExpose "Lens" ]
                |> Just
            )
        ]
    , declaration =
        \{ fieldName } ->
            { documentation = Nothing
            , name = fieldName
            , annotation =
                CodeGen.typed "Lens"
                    [ CodeGen.extRecordAnn "record"
                        [ ( fieldName, CodeGen.typeVar fieldName ) ]
                    , CodeGen.typeVar fieldName
                    ]
                    |> Just
            , implementation = getSetRecordForField fieldName
            }
    }


{-| [`FieldLensGenerator`](#FieldLensGenerator) for [z5h's zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/) in the form

    import Zipper exposing (Zipper, into)

    intoScore : Zipper { record | score : score } root -> Zipper score root
    intoScore =
        into .score (\replacement record -> { record | score = replacement })

-}
zipper : FieldLensGenerator
zipper =
    { imports =
        [ CodeGen.importStmt [ "Zipper" ]
            Nothing
            (CodeGen.exposeExplicit
                [ CodeGen.typeOrAliasExpose "Zipper"
                , CodeGen.funExpose "into"
                ]
                |> Just
            )
        ]
    , declaration =
        \{ fieldName } ->
            { documentation = Nothing
            , name = "into" ++ capitalize fieldName
            , annotation =
                let
                    zipperType focus root =
                        CodeGen.typed "Zipper" [ focus, root ]

                    fieldType =
                        CodeGen.typeVar fieldName

                    rootType =
                        CodeGen.typeVar "root"
                in
                CodeGen.funAnn
                    (zipperType
                        (CodeGen.extRecordAnn "record"
                            [ ( fieldName, fieldType ) ]
                        )
                        rootType
                    )
                    (zipperType fieldType rootType)
                    |> Just
            , implementation =
                let
                    { set, access } =
                        functionsForField fieldName
                in
                CodeGen.construct "into" [ access, set ]
            }
    }


{-| [`FieldLensGenerator`](#FieldLensGenerator) for [sjorn3's elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/) in the form

    score :
        { get : { record0 | score : score } -> score
        , set : score -> { record1 | score : score } -> { record1 | score : score }
        }
    score =
        { get = .score, set = \replacement record -> { record | score = replacement } }

-}
fields : FieldLensGenerator
fields =
    { imports = []
    , declaration =
        \{ fieldName } ->
            { documentation = Nothing
            , name = fieldName
            , annotation =
                let
                    fieldType =
                        CodeGen.typeVar "score"

                    recordType name =
                        CodeGen.extRecordAnn name
                            [ ( fieldName, fieldType ) ]
                in
                CodeGen.recordAnn
                    [ ( "get"
                      , CodeGen.funAnn (recordType "record0") fieldType
                      )
                    , ( "set"
                      , CodeGen.funAnn
                            fieldType
                            (CodeGen.funAnn (recordType "record1") (recordType "record1"))
                      )
                    ]
                    |> Just
            , implementation = getSetRecordForField fieldName
            }
    }


{-| Generate a field lens implementation in the form

    { get = .score, set = \replacement record -> { record | score = replacement } }

This is equivalent to

    let
        { access, set } =
            functionsForField fieldName
    in
    record [ ( "get", access ), ( "set", set ) ]

-}
getSetRecordForField : String -> CodeGen.Expression
getSetRecordForField fieldName =
    let
        { access, set } =
            functionsForField fieldName
    in
    CodeGen.record
        [ ( "get", access ), ( "set", set ) ]


{-| access, set and update functions for a given record field.

    functionsForField "score"

    -->
    { access = accessFun ("." ++ fieldName)
    , set =
        lambda
            [ varPattern "replacement"
            , varPattern "record"
            ]
            (update "record"
                [ ( fieldName
                  , val "replacement"
                  )
                ]
            )
    , update =
        lambda
            [ varPattern "alter"
            , varPattern "record"
            ]
            (update "record"
                [ ( fieldName
                  , applyBinOp
                        (access (val "record") fieldName)
                        piper
                        (fun "alter")
                  )
                ]
            )
    }

-}
functionsForField :
    String
    ->
        { access : CodeGen.Expression
        , set : CodeGen.Expression
        , update : CodeGen.Expression
        }
functionsForField fieldName =
    { access = CodeGen.accessFun ("." ++ fieldName)
    , set =
        CodeGen.lambda
            [ CodeGen.varPattern "replacement"
            , CodeGen.varPattern "record"
            ]
            (CodeGen.update "record"
                [ ( fieldName
                  , CodeGen.val "replacement"
                  )
                ]
            )
    , update =
        CodeGen.lambda
            [ CodeGen.varPattern "alter"
            , CodeGen.varPattern "record"
            ]
            (CodeGen.update "record"
                [ ( fieldName
                  , CodeGen.applyBinOp
                        (CodeGen.access (CodeGen.val "record") fieldName)
                        CodeGen.piper
                        (CodeGen.fun "alter")
                  )
                ]
            )
    }


{-| The provided [`FieldLensGenerator`](#FieldLensGenerator)s in this package have no documentation comment.

You can generate your own documentation, though:

    accessorsWithDocumentation { fieldName } =
        accessors { fieldName = fieldName }
            |> withDocumentation
                (emptyDocComment
                    |> markdown
                        ("Accessor for the field `." ++ fieldName ++ "`.")
                )

-}
withDocumentation :
    CodeGen.Comment CodeGen.DocComment
    -> FieldLensDeclaration
    -> FieldLensDeclaration
withDocumentation docComment generatedFieldLens =
    { generatedFieldLens
        | documentation = docComment |> Just
    }


{-| Use a different name for the generated lens.

    accessorsWithFieldSuffix { fieldName } =
        accessors { fieldName = fieldName }
            |> withName (fieldName ++ "Field")

-}
withName :
    String
    -> FieldLensDeclaration
    -> FieldLensDeclaration
withName name fieldHelperDeclaration =
    { fieldHelperDeclaration | name = name }



-- util


capitalize : String -> String
capitalize string =
    case String.uncons string of
        Just ( firstChar, after ) ->
            String.cons (Char.toUpper firstChar) after

        Nothing ->
            ""
