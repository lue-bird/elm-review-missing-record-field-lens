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

Using some update helpers:
```elm
import Record exposing (updateProjects, updateCalls)

InputAdded path newInput ->
    model
        |> updateProjects
            (ZipList.updateSelected
                (updateCalls
                    (List.map
                        (Tree.updateAt path
                            (Tree.prependChild newInput)
                        )
                    )
                )
            )
```

Using lenses:
```elm
import Field
import Accessors exposing (over)
import Accessors.Library exposing (onEach)

InputAdded path newInput ->
    model
        |> over
            ((Field.projects << ZipList.selected)
                << Field.calls
                << onEach
            )
            (Tree.updateAt path
                (Tree.prependChild newInput)
            )
```


Using methods similar to those, the code becomes more **readable**.

The biggest pain-point with these helpers is the need to manually create one for every used field.

→ This [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule automatically generates record field helpers that don't exist yet.

- In example 1: `updateProjects` & `updateCalls` will be generated in `Record.elm`
- In example 2: `Field.projects` & `Field.calls` will be generated in `Field.elm`

#### helpers that work out of the box:

- `updateField`, `setField`
- [bChiquet's elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)
- [sjorn3's elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/)
- [arturopala's elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest)
- [zh5's zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/)

Note: It's also possible to generate custom lenses or to customize the generation of existing ones.

## configuration

```elm
module ReviewConfig exposing (config)

import NoMissingRecordFieldHelper
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoMissingRecordFieldHelper.rule
        { generate = [ NoMissingRecordFieldHelper.accessors ]
        , generateIn =
            ( "Accessors", [ "Library", "RecordFields" ] )
        }
    ]
```
See [`Config`](NoMissingRecordFieldHelper#Config)

## suggestions?
→ [contributing](https://github.com/lue-bird/elm-review-missing-record-field-lens/blob/master/contributing.md).
