module VariantHelper.GenerateUsed.Testable exposing (helperDeclarationToCodeGen)

import Elm.CodeGen as CodeGen


helperDeclarationToCodeGen :
    { name : String
    , documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
    , annotation : Maybe CodeGen.TypeAnnotation
    , implementation : CodeGen.Expression
    }
    -> CodeGen.Declaration
helperDeclarationToCodeGen =
    \prismDeclaration ->
        CodeGen.funDecl
            prismDeclaration.documentation
            prismDeclaration.annotation
            prismDeclaration.name
            []
            prismDeclaration.implementation
