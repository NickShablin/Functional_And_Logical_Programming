open System

//Площадь круга
let calculateCircleArea radius = Math.PI * radius * radius

// Объема цилиндра
let calculateCylinderVolume radius height = 
    calculateCircleArea radius * height

// Суперпозиция
let mainSuperposition () =
    let readInput = Console.ReadLine >> float
    let processData = 
        (fun (r, h) -> calculateCylinderVolume r h) 
        >> (sprintf "Объем цилиндра: %.2f" : float -> string)
        >> Console.WriteLine

    printfn "Введите радиус:"
    let r = readInput()
    printfn "Введите высоту:"
    let h = readInput()
    processData (r, h) 

// Каррирование
let mainCurrying () =
    let readInput() = Console.ReadLine() |> float
    let printResult = 
        (sprintf "Объем цилиндра: %.2f" : float -> string) 
        >> Console.WriteLine

    printfn "Введите радиус:"
    let radius = readInput()
    printfn "Введите высоту:"
    let height = readInput()

    calculateCylinderVolume radius height |> printResult


printfn "Выберите режим (1 - суперпозиция, 2 - каррирование):"
match Console.ReadLine() with
| "1" -> mainSuperposition()
| "2" -> mainCurrying()
| _ -> printfn "Неверный ввод"


