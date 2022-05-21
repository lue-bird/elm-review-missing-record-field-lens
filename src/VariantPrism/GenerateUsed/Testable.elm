module VariantPrism.GenerateUsed.Testable exposing (beforeDotSuffixParser)

import Parser exposing (Parser)


beforeDotSuffixParser : String -> Parser String
beforeDotSuffixParser suffix =
    Parser.chompUntil ("." ++ suffix)
        |> Parser.getChompedString
