open System


let isBouncy number =

    match number < 100 with
    | true -> false
    | false ->
        let digits = number.ToString()
        let digitCount = digits.Length
        let rec checkDigits index hasInc hasDec =

            match (hasInc, hasDec, index = digitCount - 1) with
            | (true, true, _) -> true
            | (_, _, true) -> hasInc && hasDec
            | _ ->
                let currentDigit = digits.[index]
                let nextDigit = digits.[index + 1]

                match (nextDigit > currentDigit, nextDigit < currentDigit) with
                | (true, false) -> checkDigits (index + 1) true hasDec
                | (false, true) -> checkDigits (index + 1) hasInc true
                | _ -> checkDigits (index + 1) hasInc hasDec

        checkDigits 0 false false


let number = 1234321
let result = isBouncy number
printfn "%d, %b" number result

