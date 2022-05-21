module VariantPrism.GenerateUsed.Testable exposing (beforeSuffixParser)

import Parser exposing (Parser)


beforeSuffixParser : String -> Parser String
beforeSuffixParser suffix =
    case suffix of
        "" ->
            Parser.chompWhile (\_ -> True)
                |> Parser.getChompedString

        suffixNotEmpty ->
            Parser.chompUntil suffixNotEmpty
                |> Parser.getChompedString
