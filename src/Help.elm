module Help exposing (char0ToLower, indexed, moduleNameToString, onRow)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Location)


indexed : Int -> String -> String
indexed index =
    \name ->
        name ++ (index |> String.fromInt)


moduleNameToString : ModuleName -> String
moduleNameToString =
    String.join "."


onRow : Int -> Int -> Location
onRow row =
    \column -> { column = column, row = row }


char0ToLower : String -> String
char0ToLower =
    char0Alter Char.toLower


{-| Change the case of the first letter of a string to either uppercase or
lowercase, depending of the value of `wantedCase`. This is an internal
function for use in `toSentenceCase` and `decapitalize`.
-}
char0Alter : (Char -> Char) -> String -> String
char0Alter char0Alter_ =
    \word ->
        case String.uncons word of
            Nothing ->
                ""

            Just ( head, tail ) ->
                String.cons (char0Alter_ head) tail
