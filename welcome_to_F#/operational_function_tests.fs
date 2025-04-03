open System

let operationalFunc number func initial =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            loop (n / 10) (func acc digit)
    
    let absNumber = abs number
    if absNumber = 0 then func initial 0
    else loop absNumber initial


printfn "Сумма цифр 123: %d" (operationalFunc 123 (fun acc d -> acc + d) 0)
printfn "Произведение цифр -45: %d" (operationalFunc -45 (fun acc d -> acc * d) 1)
printfn "Минимум цифр 329: %d" (operationalFunc 329 (fun acc d -> if d < acc then d else acc) Int32.MaxValue)
printfn "Максимум цифр -702: %d" (operationalFunc -702 (fun acc d -> if d > acc then d else acc) Int32.MinValue)