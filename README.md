# elm-review-missing-record-field-lens

Often, I find myself writing code like this:
```elm
InputAdded path newInput ->
    { model
        | projects =
            model.projects
                |> ZipList.updateSelected
                    (\project ->
                        { project
                            | calls =
                                project.calls
                                    |> List.map
                                        (Tree.updateAt path
                                            (Tree.prependChild newInput)
                                        )
                        }
                    )
    }
```
which is ... suboptimal.

Using lenses:
```elm
import Field
import Accessors.Library as Accessors

InputAdded path newInput ->
    model
        |> over
            ((Field.projects << ZipList.selected)
                << Field.calls
                << Accessors.onEach
            )
            (Tree.updateAt path
                (Tree.prependChild newInput)
            )
```
the code becomes more **readable**.

The biggest pain-point with these lenses is the need to manually create one for every used field.

→ This [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule generates all those record field lenses that don't exist yet.

In this example: `Field.projects` and `Field.calls` will automatically be generated in `Field.elm`.

#### lenses that work out of the box:

- [bChiquet's elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)
- [sjorn3's elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/)
- [arturopala's elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest)
- [zh5's zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/)

Note: It's also possible to generate custom lenses or to customize existing ones.

## configuration

```elm
module ReviewConfig exposing (config)

import NoMissingRecordFieldLens
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoMissingRecordFieldLens.rule
        { generate = NoMissingRecordFieldLens.accessors
        , generateIn =
            ( "Accessors", [ "Library", "RecordFields" ] )
        }
    ]
```
See [`Config`](NoMissingRecordFieldLens#Config)

## suggestions?
→ [contributing](https://github.com/lue-bird/elm-review-missing-record-field-lens/blob/master/contributing.md).
