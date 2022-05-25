module VariantPrism.GenerateUsed.Testable exposing (prismDeclarationToCodeGen)

import Elm.CodeGen as CodeGen


prismDeclarationToCodeGen :
    { name : String
    , documentation : Maybe (CodeGen.Comment CodeGen.DocComment)
    , annotation : Maybe CodeGen.TypeAnnotation
    , implementation : CodeGen.Expression
    }
    -> CodeGen.Declaration
prismDeclarationToCodeGen =
    \prismDeclaration ->
        CodeGen.funDecl
            prismDeclaration.documentation
            prismDeclaration.annotation
            prismDeclaration.name
            []
            prismDeclaration.implementation
