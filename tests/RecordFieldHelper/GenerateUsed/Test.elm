module RecordFieldHelper.GenerateUsed.Test exposing (all)

import Expect
import RecordFieldHelper.GenerateUsed exposing (accessors, accessorsBChiquet, fields, monocle, rule, zipper)
import RecordFieldHelper.GenerateUsed.Internal exposing (nonExistentFieldLensNameInfo, printFieldLensDeclaration)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "RecordFieldHelper.GenerateUsed"
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

import Accessors exposing (Lens, makeOneToOne_)

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

import Accessors exposing (Lens, makeOneToOne_)


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

import Accessors exposing (Lens, makeOneToOne_)


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

import Accessors exposing (Lens, makeOneToOne_)

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
    Test.describe "kinds of declarations"
        [ test
            "erlandsona/elm-accessors"
            (\() ->
                accessors.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """score : Lens { record | score : score } transformed score wrap
score =
    makeOneToOne_ ".score" .score (\\alter record -> { record | score = record.score |> alter })"""
            )
        , test
            "elm-monocle"
            (\() ->
                monocle.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """score : Lens { record | score : score } score
score =
    { get = .score, set = \\replacement record -> { record | score = replacement } }"""
            )
        , test
            "elm-fields"
            (\() ->
                fields.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """score :
    { get : { record0 | score : score } -> score
    , set : score -> { record1 | score : score } -> { record1 | score : score }
    }
score =
    { get = .score, set = \\replacement record -> { record | score = replacement } }"""
            )
        , test
            "zipper"
            (\() ->
                zipper.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """intoScore : Zipper { record | score : score } root -> Zipper score root
intoScore =
    into .score (\\replacement record -> { record | score = replacement })"""
            )
        , test
            "bChiquet/elm-accessors"
            (\() ->
                accessorsBChiquet.declaration { fieldName = "score" }
                    |> printFieldLensDeclaration
                    |> Expect.equal
                        """score : Relation score transformed wrap -> Relation { record | score : score } transformed wrap
score =
    makeOneToOne .score (\\alter record -> { record | score = record.score |> alter })"""
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
