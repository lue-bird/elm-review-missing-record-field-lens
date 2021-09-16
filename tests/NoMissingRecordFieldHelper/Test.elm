module NoMissingRecordFieldHelper.Test exposing (all)

import Expect
import NoMissingRecordFieldHelper exposing (accessors, fields, monocle, rule, zipper)
import NoMissingRecordFieldHelper.Internal exposing (nonExistentFieldHelperNameInfo, printFieldHelperDeclaration)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMissingRecordFieldHelper"
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
    Accessors.over Field.score ((+) 1)
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
                                { message = nonExistentFieldHelperNameInfo "score" |> .message
                                , details = nonExistentFieldHelperNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (name, score)

import Accessors exposing (Relation, makeOneToOne)

name =
    name


score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })
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
    Accessors.over Field.score ((+) 1)
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
                                { message = nonExistentFieldHelperNameInfo "score" |> .message
                                , details = nonExistentFieldHelperNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (z, score)

import Accessors exposing (Relation, makeOneToOne)


score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })

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
    Accessors.over Field.score ((+) 1)
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
                                { message = nonExistentFieldHelperNameInfo "score" |> .message
                                , details = nonExistentFieldHelperNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (z, score)

import Accessors exposing (Relation, makeOneToOne)


score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })

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
    Accessors.over Field.score ((+) 1)
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
                                { message = nonExistentFieldHelperNameInfo "score" |> .message
                                , details = nonExistentFieldHelperNameInfo "score" |> .details
                                , under = "Accessors.Library.Fields"
                                }
                                |> Review.Test.whenFixed
                                    """module Accessors.Library.Fields exposing (a, z, score)

import Accessors exposing (Relation, makeOneToOne)

a =
    a


score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })


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
                accessors.declaration
                    |> printFieldHelperDeclaration { fieldName = "score" }
                    |> Expect.equal
                        """score : Relation score sub wrap -> Relation { record | score : score } sub wrap
score =
    makeOneToOne .score (\\f r -> { r | score = f r.score })"""
            )
        , test "elm-monocle"
            (\() ->
                monocle.declaration
                    |> printFieldHelperDeclaration { fieldName = "score" }
                    |> Expect.equal
                        """score : Lens { record | score : score } score
score =
    { get = .score, set = \\score_ r -> { r | score = score_ } }"""
            )
        , test "elm-fields"
            (\() ->
                fields.declaration
                    |> printFieldHelperDeclaration { fieldName = "score" }
                    |> Expect.equal
                        """score :
    { get : { a | score : score } -> score
    , set : score -> { b | score : score } -> { b | score : score }
    }
score =
    { get = .score, set = \\score_ r -> { r | score = score_ } }"""
            )
        , test "zipper"
            (\() ->
                zipper.declaration
                    |> printFieldHelperDeclaration { fieldName = "score" }
                    |> Expect.equal
                        """intoScore : Zipper { record | score : score } root -> Zipper score root
intoScore =
    into .score (\\score_ r -> { r | score = score_ })"""
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
    Accessors.over Field.score ((+) 1)
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
