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
Let's define some `Field.nameAlter` helpers:
```elm
module Field exposing (callsAlter, projectsAlter)
```
then
```elm
import Field

... path newInput =
    Field.projectsAlter
        (Scroll.focusMap
            (Fillable.fillMap
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
We can reduce the number of helpers by _combining the possible operations (access, replace, alter, name, ...)_ into a "lens":

```elm
import Field
import Accessors exposing (over)
import Accessors.Library exposing (onEach)

... path newInput =
    over Field.projects --← a "lens" for the field .projects
        (over Scroll.focus
            (over Hand.onFilled
                (over Fields.calls --← a "lens" for the field .calls
                    (over onEach
                        (over (Tree.elementAt path)
                            (Tree.childPrepend newInput)
                        )
                    )
                )
            )
        )
```
Seeing a pattern? You can, to put the cherry on the cake, _compose_ those "lenses":

```elm
import Field
import Hand.Extra as Hand
import Accessors exposing (over)
import Accessors.Library exposing (onEach)

... path newInput =
    over
        (Field.projects -- assumes some {m | projects : {s | focus : Hand}}
            << Scroll.focus
            << Hand.onFilled -- where type Hand = Filled {m | calls : List (Tree something)} | OtherVariant
            << Field.calls
            << onEach
            << Tree.elementAt path
        )
        (Tree.childPrepend newInput)
```

Methods like this make your code more **readable**. Compare with the first example.

→ [`NoMissingRecordFieldLens`](NoMissingRecordFieldLens) automatically generates _record field_ lenses you use.
No more manual labour.

In the last examples, `Field.projects` & `Field.calls` will be generated in `Field.elm`.

### try without installing

```bash
elm-review --template lue-bird/elm-review-missing-record-field-lens/example/field-accessors
```

### configure

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

- [bChiquet/elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)
- [sjorn3/elm-fields](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/)
- [arturopala/elm-monocle](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest)
- [zh5/zipper](https://package.elm-lang.org/packages/z5h/zipper/latest/)

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

Similarly, leaning towards a more limited, domain tailored API of types, packages, ... with strong boundaries
will lead to easier code with stronger guarantees.
[↑ example from "Make Data Structures" by Richard Feldman: `Doc.id` should be read-only](https://youtu.be/x1FU3e0sT1I?t=2745)

Don't try to design your API around lenses.
Only if the API interaction happens to mirror that behavior, Dōzo!

### when is nesting acceptable?

When parts are logically connected like an `Address` or a [`Camera`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest).
Make sure to make types, packages, ... out of these.
Don't [obsessively employ primitives](https://elm-radio.com/episode/primitive-obsession/).


## `VariantPrism.GenerateUsed`

The motivations for using this are similar to [`NoMissingRecordFieldLens`](#NoMissingRecordFieldLens),
this time trying to cut down on situations where you're only interested in values of one variant.

For any variant `type`, you can call `YourVariantType.On.oneOfThree`.
If this prism hasn't already been created, it will automatically be generated.

### try without installing

```bash
elm-review --template lue-bird/elm-review-missing-record-field-lens/example/variant-accessors
```

### configure

```elm
module ReviewConfig exposing (config)

import Review.Rule as Rule exposing (Rule)
import VariantPrism.GenerateUsed

config : List Rule
config =
    [ { name = VariantPrism.GenerateUsed.prismNameOnVariant
      , build = VariantPrism.GenerateUsed.accessors
      }
        |> VariantPrism.GenerateUsed.inVariantOriginModuleDotSuffix
            "Extra.Local"
        |> VariantPrism.GenerateUsed.importGenerationModuleAsOriginModule
        |> VariantPrism.GenerateUsed.rule
    ]
```
**Check out [`Config`](VariantPrism-GenerateUsed#Config)!**

### prisms that work out of the box

- [erlandsona/elm-accessors](https://package.elm-lang.org/packages/erlandsona/elm-accessors/latest)

It's also possible to generate custom lenses or to customize the generation of existing ones.



## suggestions?
→ [contributing](https://github.com/lue-bird/elm-review-missing-record-field-lens/blob/master/contributing.md).
