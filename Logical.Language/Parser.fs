namespace Logical.Language

module rec Parser =
    type Token = { value: string; offset: int }

    type Expr = { atoms: Atom array }

    and Atom =
        | Symbol of Token
        | Literal of Token
        | Expr of Expr

    type ParserError =
        { offset: int
          expected: char array
          actual: char option }

    let printError (error: ParserError) =
        match error.actual with
        | Some c -> printf $"expected {error.expected}, but found {error.actual} at offset {error.offset}"
        | None -> printf $"expected {error.expected}, but found empty at offset {error.offset}"

    type Stream = { code: string; offset: int }

    let peek (stream: Stream) =
        if stream.code.Length > stream.offset then
            Some stream.code[stream.offset]
        else
            None

    let forward (stream: Stream) (len: int) =
        { stream with
            offset = stream.offset + len }

    let parseLiteral (stream: Stream) =
        let rec collect chars stream =
            let head = peek stream

            match head with
            | Some c ->
                match c with
                | '\"' -> Ok(chars, forward stream 1)
                | '\\' ->
                    let next = peek (forward stream 1)

                    match next with
                    | Some escape ->
                        match escape with
                        | '\f'
                        | '\n'
                        | '\r'
                        | '\t'
                        | '\v'
                        | '\''
                        | '\"'
                        | '\\' -> collect (Array.append chars [| c; escape |]) (forward stream 2)
                        | _ ->
                            Error
                                { offset = stream.offset + 1
                                  expected = [| '\a'; '\b'; '\f'; '\n'; '\r'; '\t'; '\v'; '\''; '\"'; '\\' |]
                                  actual = None }
                    | None ->
                        Error
                            { offset = stream.offset + 1
                              expected = [| '\a'; '\b'; '\f'; '\n'; '\r'; '\t'; '\v'; '\''; '\"'; '\\' |]
                              actual = None }
                | _ -> collect (Array.append chars [| c |]) (forward stream 1)
            | None ->
                Error
                    { offset = stream.offset
                      expected = [| '\"' |]
                      actual = None }

        let head = peek stream

        match head with
        | Some c ->
            match c with
            | '\"' ->
                match collect [||] (forward stream 1) with
                | Ok(chars, stream) -> Ok(new System.String(chars), stream)
                | Error error -> Error error
            | _ ->
                Error
                    { offset = stream.offset
                      expected = [| '\"' |]
                      actual = None }
        | None ->
            Error
                { offset = stream.offset
                  expected = [| '\"' |]
                  actual = None }

    let parseSymbol (stream: Stream) =
        let rec collect chars stream =
            let head = peek stream

            match head with
            | Some c ->
                match c with
                | ')'
                | ' '
                | '\t'
                | '\r'
                | '\n' -> Ok(chars, stream)
                | _ -> collect (Array.append chars [| c |]) (forward stream 1)
            | None -> Ok(chars, stream)

        match collect [||] stream with
        | Ok(chars, stream) -> Ok(new System.String(chars), stream)
        | Error error -> Error error

    let parseAtom (stream: Stream) =
        let head = peek stream

        match head with
        | Some c ->
            match c with
            | '(' ->
                match parseExpr stream with
                | Ok(expr, stream) -> Ok(Expr expr, stream)
                | Error error -> Error error
            | '\"' ->
                let offset = stream.offset

                match parseLiteral stream with
                | Ok(str, stream) -> Ok(Literal { value = str; offset = offset }, stream)
                | Error error -> Error error
            | _ ->
                let offset = stream.offset

                match parseSymbol stream with
                | Ok(str, stream) -> Ok(Symbol { value = str; offset = offset }, stream)
                | Error error -> Error error
        | None ->
            Error
                { offset = stream.offset
                  expected = [| '(' |]
                  actual = None }

    let rec parseExpr (stream: Stream) =
        let rec collect (atoms: Atom array) (stream: Stream) =
            let head = peek stream

            match head with
            | Some c ->
                match c with
                | ')' -> Ok(atoms, forward stream 1)
                | ' '
                | '\t'
                | '\r'
                | '\n' -> collect atoms (forward stream 1)
                | _ ->
                    match parseAtom stream with
                    | Ok(atom, stream) -> collect (Array.append atoms [| atom |]) stream
                    | Error error -> Error error
            | None ->
                Error
                    { offset = stream.offset
                      expected = [| ')' |]
                      actual = None }

        let head = peek stream

        match head with
        | Some c ->
            match c with
            | '(' ->
                match collect [||] (forward stream 1) with
                | Ok(atoms, stream) -> Ok({ atoms = atoms }, stream)
                | Error error -> Error error
            | _ ->
                Error
                    { offset = stream.offset
                      expected = [| '(' |]
                      actual = Some c }
        | None ->
            Error
                { offset = stream.offset
                  expected = [| '(' |]
                  actual = None }

    let rec parse (exprs: Expr list) (stream: Stream) =
        let head = peek stream

        match head with
        | Some c ->
            match c with
            | '(' ->
                match parseExpr stream with
                | Ok(expr, stream) -> parse (List.append exprs [ expr ]) stream
                | Error error -> Error error
            | ' '
            | '\t'
            | '\r'
            | '\n' -> parse exprs (forward stream 1)
            | _ ->
                Error
                    { offset = stream.offset
                      expected = [| '(' |]
                      actual = None }
        | None -> Ok(exprs, stream)
