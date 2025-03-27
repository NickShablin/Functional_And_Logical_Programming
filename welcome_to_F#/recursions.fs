open System

// Рекурсия вверх 
let rec sumDigitsUp n =
    let absN = abs n
    if absN = 0 then 0
    else (absN % 10) + sumDigitsUp (absN / 10)


// Хвостовая рекурсия 
let sumDigitsTail n =
    let rec helper acc num =
        let absNum = abs num
        if absNum = 0 then acc
        else helper (acc + absNum % 10) (absNum / 10)
    helper 0 n


let readNumber() =
    printfn "Введите целое число:"
    match Console.ReadLine() |> Int32.TryParse with
    | (true, num) -> num
    | _ -> 
        printfn "Некорректный ввод"
        0


let main() =
    let number = readNumber()
    printfn "Рекурсия вверх: %d" (sumDigitsUp number)
    printfn "Хвостовая рекурсия: %d" (sumDigitsTail number)

main()