namespace Logical.Language

module Runtime =
    open Compiler

    let isVar (word: int) =
        let tag = tagOf word
        tag = Tag.FirstOccurrence || tag = Tag.RepeatOccurrence

    let getRef (heap: int[]) (word: int) =
        let addr = detag word
        heap[addr]

    let rec deref (heap: int[]) (word: int) =
        if isVar word then
            let r = getRef heap word
            if r = word then word else deref heap r
        else
            word

    let relocateAddr (offset: int) (addr: int) =
        let tag = tagOf addr

        if tag = Tag.FirstOccurrence || tag = Tag.RepeatOccurrence || tag = Tag.Reference then
            addr + offset
        else
            addr
