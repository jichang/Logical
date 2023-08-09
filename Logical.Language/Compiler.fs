namespace Logical.Language

module Compiler =
    open System.Text.RegularExpressions

    type Tag =
        | FirstOccurrence = 0 // V
        | RepeatOccurrence = 1 // U
        | Reference = 2 // R
        | SymbolIndex = 3 // C
        | SmallInteger = 4 // N
        | Arity = 5 // A

    let mask = 0b111

    let tag (tag: Tag) (word: int) : int = word <<< 3 ||| int tag

    let detag (word: int) : int = word >>> 3

    let tagOf (word: int) : Tag = enum<Tag> (word &&& mask)

    type Token =
        | Whitespace of string
        | Variable of string
        | Number of string
        | Atom of string
        | Keyword of string
        | Dot

    let parse (code: string) =
        let regex = Regex(@"^(\s+)|(\.)|([a-z]\w)|([A-Z_]\w)|(-?\d+)$")
        regex.Matches code

    type Clause =
        { addr: int // the base of the heap where the cells for the clause start
          len: int // the length of the code of the clause i.e., number of the heap cells the clause occupies
          neck: int // the length of the head and thus the offset where the first body element starts (or the end of the clause if none)
          gs: int array // the toplevel skeleton of a clause containing references to the location of its head and then body elements
          xs: int array } // the index vector containing dereferenced constants, numbers or array sizes as extracted from the outermost term of the head of the clause, with 0 values marking variable positions
