namespace Logical.Language.UnitTests

open Logical.Language.Compiler
open Microsoft.VisualStudio.TestTools.UnitTesting
open System

[<TestClass>]
type TestClass() =

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
               Tag.Arity |]

        for currTag in allTags do
            let tagged = tag currTag term
            let detagged = detag tagged
            let tagof = tagOf tagged
            Assert.AreEqual(term, detagged)
            Assert.AreEqual(currTag, tagof)

    [<TestMethod>]
    member this.TestParse() =
        let code = "a if b else 1000"
        printfn "%A" (parse code)
        Assert.AreEqual(1, 1)
