module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule as Rule exposing (Rule)
import VariantHelper.GenerateUsed

config : List Rule
config =
    [ VariantHelper.GenerateUsed.rule
        { build =
            VariantHelper.GenerateUsed.accessors
                { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
        , nameInModuleInternal = VariantHelper.GenerateUsed.variantAfter "on"
        , nameInModuleExternal = VariantHelper.GenerateUsed.variant
        , generationModuleIsVariantModuleDotSuffix = "On"
        }
    ]
