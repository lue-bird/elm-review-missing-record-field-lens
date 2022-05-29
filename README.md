Despite what the name suggests,
this package contains _multiple_ [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to help with automatic code generation based on use:

  - [`NoMissingRecordFieldLens`](#NoMissingRecordFieldLens)
  - [`VariantLens.GenerateUsed`](#VariantLens.GenerateUsed)

When [`lue-bird/generate-elm`](https://github.com/lue-bird/generate-elm) – a framework for making code generation easy and safe –
is finished, every functionality will be ported over.

## `NoMissingRecordFieldLens`

You find yourself writing code like ↓ ?

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

`Field.nameAlter` helpers will help remove some verbosity:

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
with
```elm
module Field exposing (callsAlter, projectsAlter)

callsAlter : (calls -> calls) -> { record | calls : calls } -> { record | calls : calls }
callsAlter alter =
    \record -> { record | calls = record.calls |> alter }
...
```

We can reduce the number of helpers by _combining the possible operations (access, replace, alter, name, ...)_ into a "lens":

```elm
import Field
import Hand.Extra.Local as Hand
import Accessors exposing (over)
import Accessors.Library exposing (onEach)

... path newInput =
    over Field.projects --← a "lens" for the field .projects
        (over Scroll.focus
            (over Hand.onFilled --← a "lens" for the variant `Hand.Filled`
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
import Hand.Extra.Local as Hand
import Accessors exposing (over)
import Accessors.Library exposing (onEach)

... path newInput =
    over                     --                  <target>
        (Field.projects      -- { _ | projects : <Scroll ...> }
            << Scroll.focus
            << Hand.onFilled -- type Hand fill = Filled <fill> | ...
            << Field.calls   -- { _ | projects : <List ...> }
            << onEach        -- List (<Tree ...>)
            << Tree.elementAt path
        )
        (Tree.childPrepend newInput)
```

Methods like this make your code more **readable**. Compare with the first example.

→ [`NoMissingRecordFieldLens`](NoMissingRecordFieldLens) automatically generates _record field_ lenses you use.

In the last examples
- `projects`, `calls` lenses will be generated in `module Field`
- `onFilled` lens will be generated in `module Hand.Extra.Local` by [`VariantLens.GenerateUsed`](#VariantLens.GenerateUsed) 

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

  - [`erlandsona/elm-accessors`](https://package.elm-lang.org/packages/erlandsona/elm-accessors/latest/)
  - [`sjorn3/elm-fields`](https://package.elm-lang.org/packages/sjorn3/elm-fields/latest/)
  - [`arturopala/elm-monocle`](https://package.elm-lang.org/packages/arturopala/elm-monocle/latest)
  - [`zh5/zipper`](https://package.elm-lang.org/packages/z5h/zipper/latest/)
  - [`bChiquet/elm-accessors`](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)

It's also possible to generate custom lenses or to customize the generation of existing ones.

### pitfalls

Don't let this pattern warp you into overusing nesting.

Structuring a model like
```elm
{ player : { position : ..., speed : ... }
, scene : { trees : ..., rocks : ... }
}
```
makes it unnecessarily hard to update inner fields.

organizing in blocks
```elm
type alias Model = 
    { column : Column
    , textPage : TextPage
    }
```

often doesn't make sense in practice
where small pieces interact with one another:
[from "Make Data Structures" by Richard Feldman – blocks → multiple sources of truth](https://youtu.be/x1FU3e0sT1I?t=1039)


```elm
{ playerPosition : ...
, playerSpeed : ...
, sceneTrees : ...
, sceneRocks : ...
}
```
Doesn't ↑ make ui harder?
Yes, but the extra explicitness is worth it.
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


## `VariantLens.GenerateUsed`

The motivations for using this are similar to [`NoMissingRecordFieldLens`](#NoMissingRecordFieldLens),
this time trying to cut down on situations where you're only interested in values of one variant.

With the [`Config`](VariantLens-GenerateUsed#Config) below,
calling `YourVariantType.onOneOfThree`,
the rule will automatically
- `import YourVariantType.Extra.Local as YourVariantType`
- generate non-existent prisms in `YourVariantType.Extra.Local`

### try without installing

```bash
elm-review --template lue-bird/elm-review-missing-record-field-lens/example/variant-accessors
```

### configure

```elm
module ReviewConfig exposing (config)

import Review.Rule as Rule exposing (Rule)
import VariantLens.GenerateUsed

config : List Rule
config =
    [ VariantLens.GenerateUsed.rule
        { build =
            VariantLens.GenerateUsed.accessors
                { valuesRepresent = VariantLens.GenerateUsed.valuesRecord }
        , name = VariantLens.GenerateUsed.prismNameVariant
        , generationModuleIsVariantModuleDotSuffix = "On"
        }
    ]
```
**Check out [`Config`](VariantLens-GenerateUsed#Config)!**

### prisms that work out of the box

- [erlandsona/elm-accessors](https://package.elm-lang.org/packages/erlandsona/elm-accessors/latest)
- [bChiquet/elm-accessors](https://package.elm-lang.org/packages/bChiquet/elm-accessors/latest)

It's also possible to generate custom prisms or to customize the generation of existing ones.


## suggestions?
→ [contributing](https://github.com/lue-bird/elm-review-missing-record-field-lens/blob/master/contributing.md)
