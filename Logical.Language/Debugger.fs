namespace Logical.Language

module Debugger =
    open Compiler

    let printCell (word: int) =
        printfn "%A [%A]:%A" word (tagOf word) (detag word)

    let printCells (cells: int array) =
        printfn "Cells"
        Array.iter printCell cells

    let printSymbol (symbol: string) = printfn "[Symbol]:%A" symbol

    let printSymbols (symbols: string array) =
        printfn "Symbols"
        Array.iter printSymbol symbols

    let printProgram (program: Program) =
        printfn "Program\n"
        printSymbols program.symbols
        printfn ""
        printCells program.cells
