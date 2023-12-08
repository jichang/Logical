namespace Logical.Language

module Engine =
    type IntArray = int32 array
    type IntList = int32 list
    type IntStack = int32 array

    type Cardinal = int32

    type Refernces = Map<string, IntStack>

    let at (refs: Refernces) (key: string) =
        match Map.tryFind key refs with
        | Some value -> value
        | None -> [||]

    // Representation of a list of goals
    type Spine =
        { head: int
          baseAddr: int
          gs: int
          trailTop: int
          k: int
          cs: int[]
          xs: int[] }

    let run = ignore