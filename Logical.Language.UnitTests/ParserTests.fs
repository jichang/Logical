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
            Ok(Literal { value = "\\\""; offset = 0 }, { code = "\"\\\"\""; offset = 4 })

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestExpr() =
        let stream = { code = "(a b c)"; offset = 0 }
        let result = parseExpr stream

        let expected: Result<Expr * Stream, ParserError> =
            Ok(
                { atoms =
                    [| Symbol { value = "a"; offset = 1 }
                       Symbol { value = "b"; offset = 3 }
                       Symbol { value = "c"; offset = 5 } |] },
                { code = "(a b c)"; offset = 7 }
            )

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestLiteralExpr() =
        let stream = { code = "(\"a\" \"b\" c)"; offset = 0 }
        let result = parseExpr stream

        let expected: Result<Expr * Stream, ParserError> =
            Ok(
                { atoms =
                    [| Literal { value = "a"; offset = 1 }
                       Literal { value = "b"; offset = 5 }
                       Symbol { value = "c"; offset = 9 } |] },
                { code = "(\"a\" \"b\" c)"
                  offset = 11 }
            )

        Assert.AreEqual(expected, result)

    [<TestMethod>]
    member this.TestNestedExpr() =
        let stream = { code = "(a (b c d) 3)"; offset = 0 }
        let result = parseExpr stream

        let expected: Result<Expr * Stream, ParserError> =
            Ok(
                { atoms =
                    [| Symbol { value = "a"; offset = 1 }
                       Expr
                           { atoms =
                               [| Symbol { value = "b"; offset = 4 }
                                  Symbol { value = "c"; offset = 6 }
                                  Symbol { value = "d"; offset = 8 } |] }
                       Symbol { value = "3"; offset = 11 } |] },
                { code = "(a (b c d) 3)"; offset = 13 }
            )

        Assert.AreEqual(expected, result)
