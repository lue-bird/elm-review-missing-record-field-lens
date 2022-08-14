module Help exposing (beforeSuffixParser, char0ToLower, char0ToUpper, declarationToString, importsToString, indexed, metaToVariantType, onColumn, qualifiedSyntaxToString, typeLens, typeRelation)

import Dict exposing (Dict)
import Elm.CodeGen as CodeGen
import Elm.Docs
import Elm.Pretty as CodeGen
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Location)
import Elm.Syntax.TypeAnnotation as Type exposing (TypeAnnotation)
import Elm.Type
import Parser exposing ((|.), Parser)
import Pretty exposing (pretty)


indexed : Int -> String -> String
indexed index =
    \name ->
        name ++ (index |> String.fromInt)


char0ToLower : String -> String
char0ToLower =
    char0Alter Char.toLower


char0ToUpper : String -> String
char0ToUpper =
    char0Alter Char.toUpper


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
                            |> Parser.backtrackable
                            |> Parser.map (\() -> Parser.Done beforeSuffixFoFar)
                        , Parser.chompIf (\_ -> True)
                            |> Parser.getChompedString
                            |> Parser.map
                                (\stillNotSuffix ->
                                    Parser.Loop (beforeSuffixFoFar ++ stillNotSuffix)
                                )
                        ]
                )


qualifiedSyntaxToString : ModuleName -> String
qualifiedSyntaxToString =
    String.join "."


onColumn : Int -> Int -> Location
onColumn column =
    \row -> { column = column, row = row }


declarationToString : CodeGen.Declaration -> String
declarationToString =
    \declaration ->
        declaration
            |> CodeGen.prettyDeclaration 100
            |> pretty 100


importsToString : List CodeGen.Import -> String
importsToString =
    \imports ->
        imports
            |> CodeGen.prettyImports
            |> pretty 1000


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


typeRelation :
    CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
typeRelation structure attribute wrap =
    CodeGen.typed "Relation" [ structure, attribute, wrap ]


{-| <https://package.elm-lang.org/packages/erlandsona/elm-accessors/latest/Accessors#Lens>
-}
typeLens :
    CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
    -> CodeGen.TypeAnnotation
typeLens structure transformed attribute built =
    CodeGen.typed "Lens" [ structure, transformed, attribute, built ]
