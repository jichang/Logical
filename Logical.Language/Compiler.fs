namespace Logical.Language

module rec Compiler =
    open Parser
    open System

    type Tag =
        | Declare = 0 // V
        | Use = 1 // U
        | Reference = 2 // R
        | Symbol = 3 // C
        | Integer = 4 // N
        | Arity = 5 // A

    let mask = 0b111

    let tag (tag: Tag) (word: int) : int = word <<< 3 ||| int tag

    let detag (word: int) : int = word >>> 3

    let tagOf (word: int) : Tag = enum<Tag> (word &&& mask)

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
        | VariableAsFunctor = 4

    type CompilerError =
        { expr: Expr option
          code: CompilerErrorCode }

    let isVariable (str: string) = Char.IsUpper str.[0]

    let equalTo (t1: 'a) = fun (t2: 'a) -> t1 = t2

    let extractFunctorName (atom: Atom) =
        match atom with
        | Variable _ -> Error CompilerErrorCode.VariableAsFunctor
        | Identifier token ->
            if String.IsNullOrEmpty token.value then
                Error CompilerErrorCode.NullOrEmptyTerm
            else
                Ok token.value
        | Expr _ -> Error CompilerErrorCode.ComplexTermAsRelation

    let clauseKey (functorName: string) (functorArity: int) = $"{functorName}/{functorArity}"

    let updateClauses (functorName: string) (functorArity: int) (clause: Clause) (clauses: Map<string, Clause array>) =
        Map.change
            (clauseKey functorName functorArity)
            (fun matchedClauses ->
                match matchedClauses with
                | Some clauses -> Some(Array.append clauses [| clause |])
                | None -> Some [| clause |])
            clauses

    let updateSymbols (symbol: string) (symbols: string array) =
        match Array.tryFindIndex (equalTo symbol) symbols with
        | Some index -> symbols
        | None -> Array.append symbols [| symbol |]

    let compileEmpty (program: Program) (expr: Expr) =
        assert (expr.atoms.Length = 0)
        Ok program

    let compileAssertion (program: Program) (expr: Expr) =
        assert (expr.atoms.Length = 1)
        let addr = program.cells.Length
        let functor = expr.atoms.[0]

        match extractFunctorName functor with
        | Ok functorName ->
            let clause =
                { addr = addr
                  len = 2
                  neck = 2
                  hgs = [| addr |]
                  xs = [||] }

            let functorArity = 0

            let clauses = updateClauses functorName functorArity clause program.clauses

            let symbols = updateSymbols functorName program.symbols

            let arityCell = tag Tag.Arity functorArity

            let functorCell = Array.findIndex (equalTo functorName) symbols |> tag Tag.Symbol

            let cells = Array.append program.cells [| arityCell; functorCell |]

            Ok
                { program with
                    cells = cells
                    clauses = clauses
                    symbols = symbols }
        | Error code -> Error { code = code; expr = Some expr }

    let compileFact (variables: Map<string, int>) (program: Program) (expr: Expr) =
        assert (expr.atoms.Length = 2)

        let addr = program.cells.Length
        let functor = expr.atoms.[0]

        match extractFunctorName functor with
        | Ok functorName ->
            let args = expr.atoms.[1]

            match args with
            | Variable argToken ->
                let functorArity = 1
                let arityCell = tag Tag.Arity functorArity

                let symbols = updateSymbols functorName program.symbols

                let functorCell = Array.findIndex (equalTo functorName) symbols |> tag Tag.Symbol

                let (variables, argCell) =
                    match Map.tryFind argToken.value variables with
                    | Some position -> (variables, tag Tag.Use position)
                    | None ->
                        let position = addr + 2
                        (variables, tag Tag.Declare position)

                let clause =
                    { addr = addr
                      len = 3
                      neck = 3
                      hgs = [| addr |]
                      xs = [||] }

                let clauses = updateClauses functorName functorArity clause program.clauses
                let cells = Array.append program.cells [| arityCell; functorCell; argCell |]

                Ok(
                    variables,
                    { program with
                        cells = cells
                        clauses = clauses
                        symbols = symbols }
                )
            | Identifier arg ->
                let functorArity = 1
                let arityCell = tag Tag.Arity functorArity

                let symbols = updateSymbols functorName program.symbols |> updateSymbols arg.value

                let functorCell = Array.findIndex (equalTo functorName) symbols |> tag Tag.Symbol

                let argCell = Array.findIndex (equalTo arg.value) symbols |> tag Tag.Symbol

                let clause =
                    { addr = addr
                      len = 3
                      neck = 3
                      hgs = [| addr |]
                      xs = [||] }

                let clauses = updateClauses functorName functorArity clause program.clauses

                let cells = Array.append program.cells [| arityCell; functorCell; argCell |]

                Ok(
                    variables,
                    { program with
                        cells = cells
                        clauses = clauses
                        symbols = symbols }
                )
            | Expr predictes ->
                let functorArity = predictes.atoms.Length
                let arityCell = tag Tag.Arity functorArity

                let symbols = updateSymbols functorName program.symbols

                let functorCell = Array.findIndex (equalTo functorName) symbols |> tag Tag.Symbol

                let predicatesCount = predictes.atoms.Length
                let predicateCells = Array.create predicatesCount 0
                let predicateAddrStart = addr + 2

                let clause =
                    { addr = addr
                      len = 3
                      neck = 3
                      hgs = [| addr |]
                      xs = [||] }

                let clauses = updateClauses functorName functorArity clause program.clauses

                let cells =
                    Array.append [| arityCell; functorCell |] predicateCells
                    |> Array.append program.cells

                let mutable predicateIndex = 0

                let initialProgram =
                    { program with
                        cells = cells
                        symbols = symbols
                        clauses = clauses }

                Array.fold
                    (fun result arg ->
                        match result with
                        | Ok(variables, program) ->
                            match arg with
                            | Identifier token ->
                                let symbols = updateSymbols functorName program.symbols |> updateSymbols token.value
                                let argCell = Array.findIndex (equalTo token.value) symbols |> tag Tag.Symbol
                                Array.set program.cells (predicateAddrStart + predicateIndex) argCell
                                predicateIndex <- predicateIndex + 1
                                Ok(variables, { program with symbols = symbols })
                            | Variable token ->
                                let (updatedVariables, argCell) =
                                    match Map.tryFind token.value variables with
                                    | Some position -> (variables, position |> tag Tag.Use)
                                    | None ->
                                        let position = predicateAddrStart + predicateIndex
                                        (Map.add token.value position variables, position |> tag Tag.Declare)

                                Array.set program.cells (predicateAddrStart + predicateIndex) argCell
                                predicateIndex <- predicateIndex + 1
                                Ok(updatedVariables, program)
                            | Expr expr ->
                                let argCell = tag Tag.Reference program.cells.Length
                                Array.set program.cells (predicateAddrStart + predicateIndex) argCell
                                predicateIndex <- predicateIndex + 1
                                compileFact variables program expr
                        | Error error -> Error error)
                    (Ok(variables, initialProgram))
                    predictes.atoms
        | Error code -> Error { code = code; expr = Some expr }

    let compileExpr (program: Program) (expr: Expr) =
        let len = expr.atoms.Length

        if len = 0 then
            compileEmpty program expr
        else if len = 1 then
            compileAssertion program expr
        else if len = 2 then
            match compileFact Map.empty program expr with
            | Ok(_, program) -> Ok program
            | Error error -> Error error
        else if len = 3 then
            let arityCells = tag Tag.Arity 3
            let atom = expr.atoms.[0]

            match extractFunctorName atom with
            | Ok clauseName ->
                Ok
                    { program with
                        cells = Array.append program.cells [| arityCells |] }
            | Error code -> Error { code = code; expr = Some expr }
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
