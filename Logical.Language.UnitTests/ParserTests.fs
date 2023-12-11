namespace Logical.Language.UnitTests

open Logical.Language.Parser
open Microsoft.VisualStudio.TestTools.UnitTesting
open System

[<TestClass>]
type TestParserClass() =
    [<TestMethod>]
    member this.TestParseLiteral() =
        let stream = { code = "\"a\""; offset = 0 }
        let result = parseLiteral stream

        let expected: Result<String * Stream, ParserError> =
            Ok("a", { code = "\"a\""; offset = 3 })

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestParseEscapedLiteral() =
        let stream = { code = "\"\\\"\""; offset = 0 }
        let result = parseLiteral stream

        let expected: Result<String * Stream, ParserError> =
            Ok("\\\"", { code = "\"\\\"\""; offset = 4 })

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestParseLiteralSymbol() =
        let stream = { code = "\"\\\"\""; offset = 0 }
        let result = parseAtom stream

        let expected: Result<Atom * Stream, ParserError> =
            Ok(Identifier { value = "\\\""; offset = 0 }, { code = "\"\\\"\""; offset = 4 })

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestExpr() =
        let stream = { code = "(A b c)"; offset = 0 }
        let result = parseExpr stream

        let expected: Result<Expr * Stream, ParserError> =
            Ok(
                { atoms =
                    [| Variable { value = "A"; offset = 1 }
                       Identifier { value = "b"; offset = 3 }
                       Identifier { value = "c"; offset = 5 } |] },
                { code = "(A b c)"; offset = 7 }
            )

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestLiteralExpr() =
        let stream = { code = "(\"A\" \"b\" c)"; offset = 0 }
        let result = parseExpr stream

        let expected: Result<Expr * Stream, ParserError> =
            Ok(
                { atoms =
                    [| Identifier { value = "A"; offset = 1 }
                       Identifier { value = "b"; offset = 5 }
                       Identifier { value = "c"; offset = 9 } |] },
                { code = "(\"A\" \"b\" c)"
                  offset = 11 }
            )

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestNestedExpr() =
        let stream = { code = "(a (B c d) 3)"; offset = 0 }
        let result = parseExpr stream

        let expected: Result<Expr * Stream, ParserError> =
            Ok(
                { atoms =
                    [| Identifier { value = "a"; offset = 1 }
                       Expr
                           { atoms =
                               [| Variable { value = "B"; offset = 4 }
                                  Identifier { value = "c"; offset = 6 }
                                  Identifier { value = "d"; offset = 8 } |] }
                       Identifier { value = "3"; offset = 11 } |] },
                { code = "(a (B c d) 3)"; offset = 13 }
            )

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestMultipleExpr() =
        let stream =
            { code = "(a (B c d) 3)\n(a B c d)"
              offset = 0 }

        let result = parse [] stream

        let expected: Result<Expr list * Stream, ParserError> =
            Ok(
                [ { atoms =
                      [| Identifier { value = "a"; offset = 1 }
                         Expr
                             { atoms =
                                 [| Variable { value = "B"; offset = 4 }
                                    Identifier { value = "c"; offset = 6 }
                                    Identifier { value = "d"; offset = 8 } |] }
                         Identifier { value = "3"; offset = 11 } |] }
                  { atoms =
                      [| Identifier { value = "a"; offset = 15 }
                         Variable { value = "B"; offset = 17 }
                         Identifier { value = "c"; offset = 19 }
                         Identifier { value = "d"; offset = 21 } |] } ],
                { code = "(a (B c d) 3)\n(a B c d)"
                  offset = 23 }
            )

        Assert.AreEqual(expected, result)
