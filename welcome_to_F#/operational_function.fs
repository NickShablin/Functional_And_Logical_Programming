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


printfn "Сумма цифр 123: %d" (operationalFunc 123 (+) 0)          
printfn "Сумма цифр -45: %d" (operationalFunc -45 (+) 0)          
printfn "Сумма цифр 0: %d" (operationalFunc 0 (+) 5)              


printfn "Произведение цифр 123: %d" (operationalFunc 123 (*) 1)   
printfn "Произведение цифр 0: %d" (operationalFunc 0 (*) 1)       


printfn "Минимум цифр 329: %d" (operationalFunc 329 min System.Int32.MaxValue)  


printfn "Максимум цифр 516: %d" (operationalFunc 516 max System.Int32.MinValue) 