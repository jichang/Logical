namespace Logical.Language

module rec Parser =
    type Token = { value: string; offset: int }

    type Expr = { atoms: Atom list }

    and Atom =
        | Token of Token
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

    let collectString (stream: Stream) =
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
                        | '\\' -> collect (Array.append chars [| escape |]) (forward stream 2)
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

        match collect [||] stream with
        | Ok chars -> Ok(string chars, stream)
        | Error error -> Error error

    let parseString (stream: Stream) =
        let head = peek stream

        match head with
        | Some c ->
            match c with
            | '\"' -> collectString (forward stream 1)
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
                match parseString stream with
                | Ok(str, stream) -> Ok(Token { value = str; offset = 0 }, stream)
                | Error error -> Error error
            | _ ->
                Error
                    { offset = stream.offset
                      expected = [| '(' |]
                      actual = None }
        | None ->
            Error
                { offset = stream.offset
                  expected = [| '(' |]
                  actual = None }

    let rec parseExpr (stream: Stream) =
        let head = peek stream

        match head with
        | Some c ->
            match c with
            | '(' -> Ok({ atoms = [] }, forward stream 1)
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
        match parseExpr stream with
        | Ok(expr, stream) -> parse (List.append exprs [ expr ]) stream
        | Error error -> Error error
