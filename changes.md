# changelog

#### 2.0.1

- documentation corrections

## 2.0.0

(mostly suggested by [@erlandsona](https://github.com/erlandsona))

- `module VariantLens.GenerateUsed` name → `VariantHelper.GenerateUsed`
    - documentation lens → helper
    - including
    - `module`-internal helper generation support
    - `ValuesRepresent` name → `ValuesCombined`
    - accessor argument `{ valuesRepresent }` name → `valuesCombined`
    - `tupleNest` name → `valuesTupleNest`
        - to be consistent with `valuesRecord`
    - `prismNameVariant` → `variant`
    - `prismNameOnVariant` → `onVariant`
- `module NoMissingRecordFieldLens` name → `RecordFieldHelper.GenerateUsed`

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
