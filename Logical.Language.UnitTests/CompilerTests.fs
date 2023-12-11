namespace Logical.Language.UnitTests

open Logical.Language.Parser
open Logical.Language.Compiler
open Microsoft.VisualStudio.TestTools.UnitTesting
open System

[<TestClass>]
type TestCompilerClass() =

    [<TestMethod>]
    member this.TestTagging() =
        let rnd = Random()
        let term = rnd.Next(0, 100)

        let allTags =
            [| Tag.FirstOccurrence
               Tag.RepeatOccurrence
               Tag.Reference
               Tag.SymbolIndex
               Tag.SmallInteger
               Tag.TermArity |]

        for currTag in allTags do
            let tagged = tag currTag term
            let detagged = detag tagged
            let tagof = tagOf tagged
            Assert.AreEqual(term, detagged)
            Assert.AreEqual(currTag, tagof)

    [<TestMethod>]
    member this.TestCompileAtomFact() =
        let stream = { code = "(a)"; offset = 0 }
        let result = parseExpr stream

        match result with
        | Ok(expr, _) ->
            let result = compileExpr Program.empty expr

            match result with
            | Ok program ->
                CollectionAssert.AreEqual([| 13; 3 |], program.cells)
                CollectionAssert.AreEqual([| "a" |], program.symbols)
                Assert.AreEqual(1, program.clauses.Count)

                CollectionAssert.AreEqual(
                    [| { addr = 0
                         len = 2
                         neck = 2
                         hgs = [| 0 |]
                         xs = [||] } |],
                    program.clauses["a"]
                )
            | Error error -> Assert.Fail $"should compile code correctly: {error.code}"
        | _ -> Assert.Fail "should parse code correctly"

    [<TestMethod>]
    member this.TestCompileLiteralFact() =
        let stream = { code = "(\"A\")"; offset = 0 }
        let result = parseExpr stream

        match result with
        | Ok(expr, _) ->
            let result = compileExpr Program.empty expr

            match result with
            | Ok program ->
                CollectionAssert.AreEqual([| 13; 3 |], program.cells)
                CollectionAssert.AreEqual([| "A" |], program.symbols)
                Assert.AreEqual(1, program.clauses.Count)

                CollectionAssert.AreEqual(
                    [| { addr = 0
                         len = 2
                         neck = 2
                         hgs = [| 0 |]
                         xs = [||] } |],
                    program.clauses["A"]
                )
            | Error error -> Assert.Fail $"should compile code correctly: {error.code}"
        | _ -> Assert.Fail "should parse code correctly"
