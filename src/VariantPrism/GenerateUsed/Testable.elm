module VariantPrism.GenerateUsed.Testable exposing (beforeSuffixParser)

import Parser exposing ((|.), Parser)


beforeSuffixParser : String -> Parser String
beforeSuffixParser suffix =
    case suffix of
        "" ->
            Parser.chompWhile (\_ -> True)
                |> Parser.getChompedString

        _ ->
            Parser.loop ""
                (\beforeSuffixFoFar ->
                    Parser.oneOf
                        [ Parser.token suffix
                            |. Parser.end
                            |> Parser.map (\() -> Parser.Done beforeSuffixFoFar)
                        , Parser.chompIf (\_ -> True)
                            |> Parser.getChompedString
                            |> Parser.map
                                (\stillNotSuffix ->
                                    Parser.Loop (beforeSuffixFoFar ++ stillNotSuffix)
                                )
                        ]
                )
