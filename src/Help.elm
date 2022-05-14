module Help exposing (indexed)


indexed : Int -> String -> String
indexed index =
    \name ->
        name ++ (index |> String.fromInt)
