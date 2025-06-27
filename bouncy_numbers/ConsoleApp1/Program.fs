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


let rec findNumber currentNumber currentBouncyCount =

    match 100 * currentBouncyCount = 99 * currentNumber with
    | true -> currentNumber
    | false ->
        let nextNumber = currentNumber + 1

        let nextBouncyCount = 
            match isBouncy nextNumber with
            | true -> currentBouncyCount + 1 
            | false -> currentBouncyCount

        findNumber nextNumber nextBouncyCount

let answer = findNumber 21780 19602
printfn "Наименьшее число, для которого доля прыгучих чисел точно равна 99%% : %d" answer

