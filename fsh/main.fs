let evens list =
   let isEven x = x%2 = 0
   List.filter isEven list


let res = evens [1;1;1;3;3;3;32;2;2;2;2;3;3;2;32;34;23;4;112;53;4523;452345;] |> List.sum


printfn "%A" res
