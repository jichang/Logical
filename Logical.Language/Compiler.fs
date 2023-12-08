namespace Logical.Language

module Compiler =
    open Parser

    type Tag =
        | FirstOccurrence = 0 // V
        | RepeatOccurrence = 1 // U
        | Reference = 2 // R
        | SymbolIndex = 3 // C
        | SmallInteger = 4 // N
        | TermArity = 5 // A

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

    type Symbol =
        | Str of string
        | Int of int

    // Representation of a clause
    type Clause =
        { addr: int // the base of the heap where the cells for the clause start
          len: int // the length of the code of the clause i.e., number of the heap cells the clause occupies
          neck: int // the length of the head and thus the offset where the first body element starts (or the end of the clause if none)
          hgs: int array // the toplevel skeleton of a clause containing references to the location of its head and then body elements
          xs: int array } // the index vector containing dereferenced constants, numbers or array sizes as extracted from the outermost term of the head of the clause, with 0 values marking variable positions

        static member empty =
            { addr = 0
              len = 0
              neck = 0
              hgs = Array.empty
              xs = Array.empty }

    type Program =
        { instructions: int array
          symbols: string array
          clauses: Map<string, Clause> }

        static member empty =
            { instructions = Array.empty
              symbols = Array.empty
              clauses = Map.empty }

    let compileExpr (program: Program) (expr: Expr) =
        let len = expr.atoms.Length
        if len = 0 then
            Ok program
        else if len = 1 then
            let arityInstruction = tag Tag.TermArity 0
            let symbol =
                match expr.atoms.[0] with
                | Symbol token ->
                    Error ""
                | Literal token ->
                    Ok program
                | Expr expr ->
                    Error ""
            Ok { program with instructions = Array.append program.instructions [| arityInstruction |] }
        else if len = 2 then
            let arityInstruction = tag Tag.TermArity 1
            Ok { program with instructions = Array.append program.instructions [| arityInstruction |] }
        else
            let rel = expr.atoms.[0]
            let args = expr.atoms.[1..len-2]
            let body = expr.atoms.[len - 1]
            Ok program

    let compile (exprs: List<Expr>) =
        let code =
            { instructions = Array.empty; clauses = Map.empty; symbols = Array.empty  }

        code
