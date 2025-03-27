open System


let rec sumDigits n =
    let absN = abs n 
    if absN = 0 then 0
    else (absN % 10) + sumDigits (absN / 10)


let readNumber() =
    printfn "Введите целое число:"
    match Console.ReadLine() |> Int32.TryParse with
    | (true, num) -> num
    | (false, _) ->
        printfn "Некорректный ввод"
        0


let main() =
    let number = readNumber()
    let result = sumDigits number
    printfn "Сумма цифр числа %d: %d" number result


main()