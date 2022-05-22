module Help.Test exposing (test)

import Expect
import Fuzz
import Help exposing (beforeSuffixParser)
import Parser
import Test exposing (Test, describe)


test : Test
test =
    describe "help"
        [ Test.fuzz
            (Fuzz.constant
                (\baseModule generationModuleSuffix ->
                    { baseModule = baseModule
                    , generationModuleSuffix = generationModuleSuffix
                    }
                )
                |> Fuzz.andMap Fuzz.string
                |> Fuzz.andMap Fuzz.string
            )
            "beforeSuffixParser"
            (\{ baseModule, generationModuleSuffix } ->
                (baseModule ++ generationModuleSuffix)
                    |> Parser.run (beforeSuffixParser generationModuleSuffix)
                    |> Expect.equal (Ok baseModule)
            )
        ]
