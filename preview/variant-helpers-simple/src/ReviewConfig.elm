module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule as Rule exposing (Rule)
import VariantHelper.GenerateUsed exposing (VariantHelperNameConfig)
import Elm.CodeGen as CodeGen


config : List Rule
config =
    [ VariantHelper.GenerateUsed.rule
        { nameInModuleInternal = variantAfter "to"
        , nameInModuleExternal = variantAfter "to"
        , build =
            variantAccessTry
                { valuesCombined = VariantHelper.GenerateUsed.valuesRecord }
        , generationModuleIsVariantModuleDotSuffix = "Help"
        }
    ]



variantAccessTry :
    { valuesCombined : ValuesCombined }
    -> VariantHelperBuild
variantAccessTry { valuesCombined } =
    \{ typeName, variantName, variantValues, typeParameters } ->
        let
            generation =
                if otherVariants |> Dict.isEmpty then
                    VariantHelper.GenerateUsed.variantOnly
                        { name = variantName
                        , values = variantValues
                        , valuesCombined = valuesCombined
                        }
                
                else
                    VariantHelper.GenerateUsed.variantInMultiple
                        { name = variantName
                        , values = variantValues
                        , valuesCombined = valuesCombined
                        }
        in
        { imports = []
        , documentation =
            CodeGen.emptyDocComment
                |> CodeGen.markdown
                    ([ "Try to access the values on the variant `"
                     , typeName
                     , "."
                     , variantName
                     , "`"
                     ]
                        |> String.concat
                    )
                |> Just
        , annotation =
            CodeGen.funAnn
                (CodeGen.typed typeName
                    (typeParameters |> List.map CodeGen.typeVar)
                )
                (CodeGen.maybeAnn
                    generation.typeValues
                )
                |> Just
        , implementation =
            generation.access
        }
