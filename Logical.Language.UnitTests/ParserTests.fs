namespace Logical.Language.UnitTests

open Logical.Language.Parser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestParserClass() =
    [<TestMethod>]
    member this.TestParseString() =
        let stream = { code = "\"\""; offset = 0 }
        let result = parseString stream
        Assert.IsTrue(Result.isOk result)

    [<TestMethod>]
    member this.TestParseEscapedString() =
        let stream = { code = "\"\\\"\""; offset = 0 }
        let result = parseString stream
        Assert.IsTrue(Result.isOk result)
