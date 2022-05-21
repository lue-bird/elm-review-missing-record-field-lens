module Help exposing (char0ToLower, indexed, metaToVariantType, moduleNameToString, onRow)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location)
import Elm.Syntax.TypeAnnotation as Type exposing (TypeAnnotation)
import Elm.Type


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


metaToType : Elm.Type.Type -> TypeAnnotation
metaToType =
    \type_ ->
        let
            step =
                \typeValue ->
                    typeValue |> metaToType |> Node Range.emptyRange
        in
        case type_ of
            Elm.Type.Var name ->
                Type.GenericType name

            Elm.Type.Lambda argument result ->
                Type.FunctionTypeAnnotation
                    (argument |> step)
                    (result |> step)

            Elm.Type.Tuple parts ->
                Type.Tupled (parts |> List.map step)

            Elm.Type.Type tag arguments ->
                Type.Typed
                    (tag |> qualifiedToSyntax |> Node Range.emptyRange)
                    (arguments |> List.map step)

            Elm.Type.Record fields maybeExtended ->
                let
                    fieldTypes =
                        fields
                            |> List.map
                                (\( tag, value ) ->
                                    ( tag |> Node Range.emptyRange, value |> step )
                                        |> Node Range.emptyRange
                                )
                in
                case maybeExtended of
                    Nothing ->
                        Type.Record fieldTypes

                    Just extended ->
                        Type.GenericRecord
                            (extended |> Node Range.emptyRange)
                            (fieldTypes |> Node Range.emptyRange)


metaToVariantType :
    Elm.Docs.Union
    ->
        ( String
        , { variants : Dict String (List TypeAnnotation)
          , parameters : List String
          }
        )
metaToVariantType =
    \variantType ->
        ( variantType.name
        , { parameters = variantType.args
          , variants =
                variantType.tags
                    |> Dict.fromList
                    |> Dict.map (\_ -> List.map metaToType)
          }
        )


qualifiedToSyntax : String -> ( ModuleName, String )
qualifiedToSyntax =
    \joined ->
        case joined |> String.split "." |> List.reverse of
            [] ->
                ( [], "" )

            name :: qualificationReverse ->
                ( qualificationReverse |> List.reverse, name )
