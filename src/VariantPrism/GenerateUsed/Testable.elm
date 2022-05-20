module VariantPrism.GenerateUsed.Testable exposing (generationModuleParser)

import Parser exposing ((|=), Parser)


generationModuleParser : Parser { baseModule : String }
generationModuleParser =
    Parser.succeed (\baseModule -> { baseModule = baseModule })
        |= (Parser.chompUntil ".On"
                |> Parser.getChompedString
           )
