module NoMissingRecordFieldLens exposing
    ( rule, Config
    , accessors, monocle, fields, zipper
    , FieldLensGenerator, FieldLensDeclaration, functionsForField, getSetRecordForField, withDocumentation, withName
    )

{-|

@docs rule, Config


# lens generators


## working out of the box

@docs accessors, monocle, fields, zipper


## custom

@docs FieldLensGenerator, FieldLensDeclaration, functionsForField, getSetRecordForField, withDocumentation, withName

-}

import Elm.CodeGen as CodeGen
import NoMissingRecordFieldLens.Internal as Internal
import Review.Rule exposing (Rule)


{-| Reports record field lenses that are called in the code but don't exist yet and automatically generates them.

Examples of such lenses are

  - [bChiquet's elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)
  - [sjorn3's elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/)
  - [arturopala's elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest)
  - [zh5's zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/)

```
config =
    [ NoMissingRecordFieldLens.rule
        { generate = NoMissingRecordFieldLens.accessors
        , generateIn = ( "Accessors", [ "Library", "Fields" ] )
        }
    ]
```

  - `generate`: What kind of lens to generate:
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
        Accessors.over Field.score ((+) 1)


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
      - [a custom one](#FieldLensGenerator).

  - `generateIn`: The module where all field lenses will be generated in

    understand `( "Module", [ "Name" ] )` as `Module.Name`

-}
type alias Config =
    { generator : FieldLensGenerator
    , generateIn : ( String, List String )
    }


{-| How to generate a [`FieldLensDeclaration`](#FieldLensDeclaration) plus the necessary imports.

Out of the box there are lenses for

  - [`elm-accessors`](#accessors),
  - [`elm-fields`](#fields),
  - [`elm-monocle`](#monocle),
  - [`zipper`](#zipper)

You can also create a custom one with the help of [the-sett's elm-syntax-dsl](https://package.elm-lang.org/packages/the-sett/elm-syntax-dsl/latest):

    customLens : FieldLensGenerator
    customLens  =
        { imports =
            [ importStmt [ "CustomLens" ]
                Nothing
                (exposeExplicit
                    [ typeOrAliasExpose "CustomLens" ]
                    |> Just
                )
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


{-| Generate lenses for [bChiquet's elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest) in the form

    import Accessors exposing (Relation, makeOneToOne)

    score : Relation score sub wrap -> Relation { record | score : score } sub wrap
    score =
        makeOneToOne .score (\f r -> { r | score = f r.score })

-}
accessors : FieldLensGenerator
accessors =
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
                        CodeGen.typed "Relation"
                            [ super
                            , CodeGen.typeVar "sub"
                            , CodeGen.typeVar "wrap"
                            ]
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


{-| Generate lenses for [arturopala's elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest) in the form

    import Monocle.Lens exposing (Lens)

    score : Lens { record | score : score } score
    score =
        { get = .score, set = \score_ r -> { r | score = score_ } }

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


{-| Generate lenses for [z5h's zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/) in the form

    import Zipper exposing (Zipper, into)

    intoScore : Zipper { record | score : score } root -> Zipper score root
    intoScore =
        into .score (\score_ r -> { r | score = score_ })

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


{-| Generate lenses for [sjorn3's elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/) in the form

    score :
        { get : { a | score : score } -> score
        , set : score -> { b | score : score } -> { b | score : score }
        }
    score =
        { get = .score, set = \score_ r -> { r | score = score_ } }

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
                      , CodeGen.funAnn (recordType "a") fieldType
                      )
                    , ( "set"
                      , CodeGen.funAnn
                            fieldType
                            (CodeGen.funAnn (recordType "b") (recordType "b"))
                      )
                    ]
                    |> Just
            , implementation = getSetRecordForField fieldName
            }
    }


{-| Generate a field lens implementation in the form

    { get = .score, set = \score_ r -> { r | score = score_ } }

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


{-| The access, set and update functions for a record field.

    functionsForField "score"
    --> { access = accessFun ".score"
    --> , set =
    -->     lambda
    -->         [ varPattern "score_", varPattern "r" ]
    -->         (update "r" [ ( "score", val "score_" ) ])
    --> , update =
    -->     lambda
    -->         [ varPattern "f", varPattern "r" ]
    -->         (update "r"
    -->             [ ( "score"
    -->               , construct "f" [ access (val "r") "score" ]
    -->               )
    -->             ]
    -->         )
    --> }

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
            [ CodeGen.varPattern (fieldName ++ "_")
            , CodeGen.varPattern "r"
            ]
            (CodeGen.update "r"
                [ ( fieldName
                  , CodeGen.val (fieldName ++ "_")
                  )
                ]
            )
    , update =
        CodeGen.lambda
            [ CodeGen.varPattern "f"
            , CodeGen.varPattern "r"
            ]
            (CodeGen.update "r"
                [ ( fieldName
                  , CodeGen.construct "f"
                        [ CodeGen.access (CodeGen.val "r") fieldName ]
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
withName name fieldLensDeclaration =
    { fieldLensDeclaration | name = name }



-- util


capitalize : String -> String
capitalize string =
    case String.uncons string of
        Just ( firstChar, after ) ->
            String.cons (Char.toUpper firstChar) after

        Nothing ->
            ""
