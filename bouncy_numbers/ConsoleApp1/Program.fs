open System

let isBouncy n =
    if n < 100 then 
        false
    else
        let s = n.ToString()
        let len = s.Length
        let rec check i hasInc hasDec =
            if hasInc && hasDec then 
                true
            elif i = len - 1 then 
                hasInc && hasDec
            else
                let a = s.[i]
                let b = s.[i+1]
                if b > a then 
                    check (i+1) true hasDec
                elif b < a then
                    check (i+1) hasInc true
                else
                    check (i+1) hasInc hasDec
        check 0 false false


let number = 1234321
let result = isBouncy number
printfn "%d, %b" number result