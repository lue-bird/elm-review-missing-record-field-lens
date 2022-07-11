## 2.0.0 plans

- variant lens name → prism
    - including `module VariantLens.GenerateUsed` name → `VariantPrism.GenerateUsed`
- `module`-internal prism generation support
    - name configuration add

# changelog

### 1.1.0

- in `NoMissingRecordFieldLens`
    - `NoMissingRecordFieldLens.accessors` changed to generate for [`erlandsona/elm-accessors`](https://dark.elm.dmy.fr/packages/erlandsona/elm-accessors/latest/)
    - `NoMissingRecordFieldLens.accessorsBChiquet` added
    - changed to more descriptive generation names
- added `VariantLens.GenerateUsed` (initiated by [@erlandsona](https://github.com/erlandsona))

#### 1.0.2

- example `import Accessors exposing (over)` added

#### 1.0.1

- `NoMissingRecordFieldLens.withName` documentation corrected (thanks [@jfmengels](https://github.com/jfmengels)!)
