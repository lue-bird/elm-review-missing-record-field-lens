module NoMissingRecordFieldLens.Test exposing (all)

import Expect
import NoMissingRecordFieldLens exposing (accessors, fields, monocle, rule, zipper)
import NoMissingRecordFieldLens.Internal exposing (nonExistentFieldLensNameInfo, printFieldLensDeclaration)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingRecordFieldLens"
        [ fail, success, declarations ]


fail : Test
fail =
    describe "report"
        [ test "field lens that doesn't exist and add it after existing declarations"
            (\() ->
                [ """module Accessors.Library.Fields exposing (name)

name =
    name
"""
                , """module OtherModule exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score (\\score -> score + 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (rule
                            { generator = accessors
                            , generateIn = ( "Accessors", [ "Library", "Fields" ] )
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Accessors.Library.Fields"
                          , [ Review.Test.error
                                { message = nonExistentFieldLensNameInfo "score" |> .message
                                , details = nonExistentFieldLensNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (name, score)

import Accessors exposing (Relation, makeOneToOne)

name =
    name


score : Lens { record | score : score } transformed score wrap
score =
    makeOneToOne_ ".score" .score (\\alter record -> { record | score = record.score |> alter })
"""
                            ]
                          )
                        ]
            )
        , test "field lens that doesn't exist and add it before existing declarations after module definition"
            (\() ->
                [ """module Accessors.Library.Fields exposing (z)

z =
    z
"""
                , """module OtherModule exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score (\\score -> score + 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (rule
                            { generator = accessors
                            , generateIn = ( "Accessors", [ "Library", "Fields" ] )
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Accessors.Library.Fields"
                          , [ Review.Test.error
                                { message = nonExistentFieldLensNameInfo "score" |> .message
                                , details = nonExistentFieldLensNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (z, score)

import Accessors exposing (Relation, makeOneToOne)


score : Lens { record | score : score } transformed score wrap
score =
    makeOneToOne_ ".score" .score (\\alter record -> { record | score = record.score |> alter })

z =
    z
"""
                            ]
                          )
                        ]
            )
        , test "field lens that doesn't exist and add it before existing declarations after import"
            (\() ->
                [ """module Accessors.Library.Fields exposing (z)

z =
    z
"""
                , """module OtherModule exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score (\\score -> score + 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (rule
                            { generator = accessors
                            , generateIn = ( "Accessors", [ "Library", "Fields" ] )
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Accessors.Library.Fields"
                          , [ Review.Test.error
                                { message = nonExistentFieldLensNameInfo "score" |> .message
                                , details = nonExistentFieldLensNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (z, score)

import Accessors exposing (Relation, makeOneToOne)


score : Lens { record | score : score } transformed score wrap
score =
    makeOneToOne_ ".score" .score (\\alter record -> { record | score = record.score |> alter })


z =
    z
"""
                            ]
                          )
                        ]
            )
        , test "field lens that doesn't exist and add it between existing declarations"
            (\() ->
                [ """module Accessors.Library.Fields exposing (a, z)

a =
    a


z =
    z
"""
                , """module OtherModule exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score (\\score -> score + 1)
"""
                ]
                    |> Review.Test.runOnModules
                        (rule
                            { generator = accessors
                            , generateIn = ( "Accessors", [ "Library", "Fields" ] )
                            }
                        )
                    |> Review.Test.expectErrorsForModules
                        [ ( "Accessors.Library.Fields"
                          , [ Review.Test.error
                                { message = nonExistentFieldLensNameInfo "score" |> .message
                                , details = nonExistentFieldLensNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (a, z, score)

import Accessors exposing (Relation, makeOneToOne)

a =
    a


score : Lens { record | score : score } transformed score wrap
score =
    makeOneToOne_ ".score" .score (\\alter record -> { record | score = record.score |> alter })


z =
    z
"""
                            ]
                          )
                        ]
            )
        ]


declarations : Test
declarations =
    describe "kinds of declarations"
        [ test "elm-accessors"
            (\() ->
                accessors.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """score : Lens { record | score : score } transformed score wrap
score =
    makeOneToOne_ ".score" .score (\\alter record -> { record | score = record.score |> alter })"""
            )
        , test "elm-monocle"
            (\() ->
                monocle.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """score : Lens { record | score : score } transformed score wrap
score =
    makeOneToOne_ ".score" .score (\\alter record -> { record | score = record.score |> alter })"""
            )
        , test "elm-fields"
            (\() ->
                fields.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """score :
    { get : { a | score : score } -> score
    , set : score -> { b | score : score } -> { b | score : score }
    }
score =
    { get = .score, set = \\score_ record -> { record | score = score_ } }"""
            )
        , test "zipper"
            (\() ->
                zipper.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """intoScore : Zipper { record | score : score } root -> Zipper score root
intoScore =
    into .score (\\score_ record -> { record | score = score_ })"""
            )
        ]


success : Test
success =
    test "doesn't report existing field lens"
        (\() ->
            [ """module Accessors.Library.Fields exposing (score)

score =
    score
"""
            , """module OtherModule exposing (scoreAPoint)

import Accessors.Library.Fields as Field

scoreAPoint =
    Accessors.over Field.score (\\score -> score + 1)
       """
            ]
                |> Review.Test.runOnModules
                    (rule
                        { generator = accessors
                        , generateIn = ( "Accessors", [ "Library", "Fields" ] )
                        }
                    )
                |> Review.Test.expectNoErrors
        )
