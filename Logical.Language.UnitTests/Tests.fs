namespace Logical.Language.UnitTests

open Logical.Language.Lang
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
            printf "%A %A %A %A %A\n" term currTag tagged detagged tagof
            Assert.AreEqual(term, detagged)
            Assert.AreEqual(currTag, tagof)
