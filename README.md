Despite what the name suggests,
this package contains _multiple_ [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to help with automatic code generation based on use:

  - [`NoMissingRecordFieldLens`](#NoMissingRecordFieldLens)
  - [`VariantPrism.GenerateUsed`](#VariantPrism.GenerateUsed)

When [`lue-bird/generate-elm`](https://github.com/lue-bird/generate-elm) – a framework for making code generation easy and safe –
is finished, every functionality will be ported over.

## `NoMissingRecordFieldLens`

You find myself writing code like this?

```elm
... path newInput =
    \state ->
        { state
            | projects =
                state.projects
                    |> Scroll.focusMap
                        (Fillable.map
                            (\project ->
                                { project
                                    | calls =
                                        project.calls
                                            |> List.map
                                                (Tree.elementAlter
                                                    ( path, Tree.childPrepend newInput )
                                                )
                                }
                            )
                        )
        }
```
Let's use some `fieldAlter` helpers:
```elm
import Field

... path newInput =
    Field.projectsAlter
        (Scroll.focusMap
            (Fillable.map
                (Field.callsAlter
                    (List.map
                        (Tree.elementAlter
                            ( path, Tree.childPrepend newInput )
                        )
                    )
                )
            )
        )
```
We can reduce the number of helpers by _combining the possible operations_ into a "lens":

```elm
import Field exposing (projects, calls)
import Accessors exposing (over)
import Accessors.Library exposing (onEach)

... path newInput =
    over Field.projects
        (over Scroll.focus
            (over Fillable.try
                (over fields.calls
                    (over onEach
                        (over (Tree.elementAt path)
                            (Tree.childPrepend newInput)
                        )
                    )
                )
            )
        )
```
Seeing a pattern? Let's _compose_ those "lenses":

```elm
import Field
import Accessors exposing (over)
import Accessors.Library exposing (onEach)

... path newInput =
    over
        ((Field.projects << Scroll.focus << Fillable.onFilled)
            << Field.calls
            << onEach
            << Tree.elementAt path
        )
        (Tree.childPrepend newInput)
```

Say what you want, but methods like this make your code more **readable**.

→ This rule automatically generates record field lenses you use. No more painful manual labour.

In the last examples, `Field.projects` & `Field.calls` will be generated in `Field.elm`

### try without installing

```bash
elm-review --template lue-bird/elm-review-missing-record-field-lens/example/field-accessors
```

### configuration

```elm
module ReviewConfig exposing (config)

import NoMissingRecordFieldLens
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoMissingRecordFieldLens.rule
        { generator = NoMissingRecordFieldLens.accessors
        , generateIn = ( "Field", [] )
        }
    ]
```
See [`Config`](NoMissingRecordFieldLens#Config)

### lenses that work out of the box

- [bChiquet's elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)
- [sjorn3's elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/)
- [arturopala's elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest)
- [zh5's zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/)

It's also possible to generate custom lenses or to customize the generation of existing ones.

### pitfalls

Don't let this pattern warp you into overusing nesting.

Structuring a model like
```elm
{ player : { position : ..., speed : ... }
, scene : { trees : ..., rocks : ... }
}


```
is a smelly pattern. It makes it unnecessarily hard to update inner fields.
```elm
{ playerPosition : ...
, playerSpeed : ...
, sceneTrees : ...
, sceneRocks : ...
}
```
Doesn't this make ui harder? Yes, but the extra explicitness is worth it.
`player` could have things that are irrelevant to the ui like `configuredControls` etc.
It's best to keep state structure and ui requirements separate.

### when is nesting acceptable?

When parts are logically connected like an `Address` or a [`Camera`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest).
Make sure to make types out of these.
Don't [obsessively employ primitives](https://elm-radio.com/episode/primitive-obsession/).


## `VariantPrism.GenerateUsed`

The motivations for using this are similar to [`NoMissingRecordFieldLens`](#NoMissingRecordFieldLens),
this time trying to cut down on situations where you're only interested in values of one variant.

For any variant `type`, you can call `YourVariantType.Variant.oneOfThree`.
If this prism hasn't already been created, it will automatically be generated.

### try without installing

```bash
elm-review --template lue-bird/elm-review-missing-record-field-lens/example/variant-accessors
```

### configuration

```elm
module ReviewConfig exposing (config)

import VariantPrism.GenerateUsed
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ VariantPrism.GenerateUsed.rule
        VariantPrism.GenerateUsed.accessors
    ]
```
See [`Config`](VariantPrism-GenerateUsed#Config)

### prisms that work out of the box

- [erlandsona's elm-accessors](https://package.elm-lang.org/packages/erlandsona/elm-accessors/latest)

It's also possible to generate custom lenses or to customize the generation of existing ones.



## suggestions?
→ [contributing](https://github.com/lue-bird/elm-review-missing-record-field-lens/blob/master/contributing.md).
