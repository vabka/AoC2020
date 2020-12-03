let parseLine (line: string) =
    line
    |> Seq.mapi (fun n v -> if v = '.' then 0 else 1 <<< n)
    |> Seq.reduce (+)


// stealed from question https://stackoverflow.com/questions/963129/there-is-any-c-sharp-bitwise-shift-operator-that-moves-the-overflown-bits-to-the
let wrappingShiftLeft numberOfBits value =
    let r = (value >>> (31 - numberOfBits))
    if r = 0 then value <<< numberOfBits else r

let map =
    System.IO.File.ReadAllLines("input.txt")
    |> Seq.map parseLine
    |> Seq.toList

let count right down =
    let mutable currentPosition = 1
    let mutable count = 0L
    let shift3 = wrappingShiftLeft right
    
    for i in [0..down..(map.Length-1)] do
        let line = map.[i]
        if (currentPosition &&& line) > 0 then count <- count + 1L
        currentPosition <- shift3 currentPosition
    count


let result = count 3 1
printfn "%d" result