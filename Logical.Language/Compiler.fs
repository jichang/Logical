namespace Logical.Language

module Compiler =
    open Parser
    open System

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
        { cells: int array
          symbols: string array
          clauses: Map<string, Clause array> }

        static member empty =
            { cells = Array.empty
              symbols = Array.empty
              clauses = Map.empty }

    type CompilerErrorCode =
        | Unknown = 0
        | WrongFormat = 1
        | NullOrEmptyTerm = 2
        | ComplexTermAsRelation = 3
        | VariableAsFact = 4

    type CompilerError =
        { expr: Expr option
          code: CompilerErrorCode }

    let isVariable (str: string) = Char.IsUpper str.[0]

    let equalTo (t1: 'a) = fun (t2: 'a) -> t1 = t2

    let compileExpr (program: Program) (expr: Expr) =
        let len = expr.atoms.Length

        if len = 0 then
            Ok program
        else if len = 1 then
            let arityCell = tag Tag.TermArity 1
            let term = expr.atoms.[0]

            let clauseAtom =
                match term with
                | Literal token ->
                    if String.IsNullOrEmpty token.value then
                        Error
                            { expr = Some expr
                              code = CompilerErrorCode.NullOrEmptyTerm }
                    else
                        Ok token.value
                | Symbol token ->
                    if String.IsNullOrEmpty token.value then
                        Error
                            { expr = Some expr
                              code = CompilerErrorCode.NullOrEmptyTerm }
                    else if isVariable token.value then
                        Error
                            { expr = Some expr
                              code = CompilerErrorCode.VariableAsFact }
                    else
                        Ok token.value
                | Expr expr ->
                    Error
                        { expr = Some expr
                          code = CompilerErrorCode.ComplexTermAsRelation }

            match clauseAtom with
            | Ok clauseName ->
                let clause =
                    { addr = program.cells.Length
                      len = 2
                      neck = 2
                      hgs = [| program.cells.Length |]
                      xs = [||] }

                let clauses =
                    Map.change
                        clauseName
                        (fun matchedClauses ->
                            match matchedClauses with
                            | Some clauses -> Some(Array.append clauses [| clause |])
                            | None -> Some [| clause |])
                        program.clauses

                let symbols = Array.append program.symbols [| clauseName |]

                match Array.tryFindIndex (equalTo clauseName) program.symbols with
                | Some index ->
                    let headCell = tag Tag.SymbolIndex index

                    Ok
                        { program with
                            cells = Array.append program.cells [| arityCell; headCell |]
                            clauses = clauses
                            symbols = symbols }
                | None ->
                    let headCell = tag Tag.SymbolIndex program.symbols.Length

                    Ok
                        { program with
                            cells = Array.append program.cells [| arityCell; headCell |]
                            clauses = clauses
                            symbols = symbols }
            | Error error -> Error error
        else if len = 2 then
            let arityCells = tag Tag.TermArity 2

            Ok
                { program with
                    cells = Array.append program.cells [| arityCells |] }
        else if len = 3 then
            let arityCells = tag Tag.TermArity 3

            Ok
                { program with
                    cells = Array.append program.cells [| arityCells |] }
        else
            Error
                { expr = Some expr
                  code = CompilerErrorCode.WrongFormat }

    let compile (exprs: List<Expr>) =
        let code =
            { cells = Array.empty
              clauses = Map.empty
              symbols = Array.empty }

        code
