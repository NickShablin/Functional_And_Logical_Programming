// Задание 1,2

open System

let rec readList n = 
    match n with
    | 0 -> []
    | _ ->
        let head = Console.ReadLine() |> int
        let tail = readList (n-1)
        head::tail

let churchList = readList 7

let printChurchList list =
    Console.WriteLine("Church list:")
    let rec writeElements = function
        | [] -> 0
        | (head: int)::tail -> 
            Console.WriteLine(head)
            writeElements tail
    writeElements list

// Задание 3

let foldFiltered (list: int list) (f: int -> int -> int) (p: int -> bool) (acc: int) =
    let rec loop lst currentAcc =
        match lst with
        | [] -> currentAcc
        | head::tail when p head -> loop tail (f currentAcc head)
        | _::tail -> loop tail currentAcc
    loop list acc

// Задание 4

let sumEvenNumbers list = 
    foldFiltered list (+) (fun x -> x % 2 = 0) 0

let findMin list =
    let minFunc x y = 
        match x < y with
        | true -> x
        | false -> y
    match list with
    | [] -> failwith "Empty list"
    | head::tail -> foldFiltered tail minFunc (fun _ -> true) head

let countOdd list =
    foldFiltered list (fun acc _ -> acc + 1) (fun x -> x % 2 <> 0) 0

// Задание 5

let findMostFrequent list =
    let rec countFrequencies lst acc =
        match lst with
        | [] -> acc
        | head::tail ->
            let updatedAcc = 
                match List.tryFind (fun (x, _) -> x = head) acc with
                | Some (x, count) -> (x, count + 1) :: List.filter (fun (y, _) -> y <> head) acc
                | None -> (head, 1) :: acc
            countFrequencies tail updatedAcc
    
    let rec findMaxFrequency maxElem maxCount = function
        | [] -> maxElem
        | (elem, count)::tail when count > maxCount -> findMaxFrequency elem count tail
        | _::tail -> findMaxFrequency maxElem maxCount tail
    
    match list with
    | [] -> failwith "Empty list"
    | _ ->
        let frequencies = countFrequencies list []
        let (mostFreq, count) = List.head frequencies
        findMaxFrequency mostFreq count (List.tail frequencies)

// Задание 6

type BinaryTree =
    | Leaf
    | Node of string * BinaryTree * BinaryTree

let rec insert value tree =
    match tree with
    | Leaf -> Node(value, Leaf, Leaf)
    | Node(v, left, right) ->
        match value < v with
        | true -> Node(v, insert value left, right)
        | false -> Node(v, left, insert value right)

let listToTree list =
    list
    |> List.map string
    |> List.fold (fun acc elem -> insert elem acc) Leaf

let printInOrder tree =
    let rec loop t cont =
        match t with
        | Leaf -> cont()
        | Node(v, left, right) ->
            loop left (fun () -> 
                printfn "Tree node: %s" v
                loop right cont)
    loop tree (fun () -> ())

// Задание 7

let findMostFrequentList (list: int list) =
    list
    |> List.countBy id
    |> List.maxBy snd
    |> fst

// Задание 8

let countSquareElements list =
    list
    |> List.filter (fun x -> 
        list 
        |> List.exists (fun y -> 
            match y * y = x with
            | true -> true
            | false -> false)
    )
    |> List.length

// Задание 9

let sumDigits n =
    let rec loop x acc =
        match x with
        | 0 -> acc
        | _ -> loop (x / 10) (acc + x % 10)
    loop (abs n) 0

let countDivisors n =
    let rec loop i count =
        match i > abs n with
        | true -> count
        | false -> 
            match n % i = 0 with
            | true -> loop (i + 1) (count + 1)
            | false -> loop (i + 1) count
    match n with
    | 0 -> 0
    | _ -> loop 1 0

let createTriples a b c =
    let sortedA = a |> List.sortDescending

    let sortedB = 
        b 
        |> List.sortBy (fun x -> (sumDigits x, -abs x))
        |> List.indexed
        |> List.sortBy fst
        |> List.map snd

    let sortedC = 
        c 
        |> List.sortByDescending (fun x -> (countDivisors x, -abs x))
        |> List.indexed
        |> List.sortByDescending fst
        |> List.map snd
    
    List.zip3 sortedA sortedB sortedC

// Задание 10

let rec readStringList n = 
    match n with
    | 0 -> []
    | _ ->
        let head = Console.ReadLine()
        let tail = readStringList (n-1)
        head::tail

let sortStringsByLength list =
    list |> List.sortBy String.length



// Задание 11 (1.10) Даны два массива. Необходимо найти количество совпадающих по значению элементов.
let countCommonElements list1 list2 =
    list1 
    |> List.filter (fun x -> List.contains x list2)
    |> List.length

// Задание 12 (1.20) Дан целочисленный массив. Необходимо найти все пропущенные числа.
let findMissingNumbers list =
    match list with
    | [] -> []
    | _ ->
        let minVal = List.min list
        let maxVal = List.max list
        [minVal..maxVal] 
        |> List.filter (fun x -> not (List.contains x list))

// Задание 13 (1.30) Дан целочисленный массив и натуральный индекс (число, меньшее размера массива). Необходимо определить является ли элемент по указанному индексу локальным максимумом.
let isLocalMax list index =
    let rec check idx prev current next =
        match idx with
        | 0 -> current > next
        | _ when idx = List.length list - 1 -> current > prev
        | _ -> current > prev && current > next
    match List.tryItem index list with
    | None -> false
    | Some current ->
        let prev = List.tryItem (index - 1) list |> Option.defaultValue current
        let next = List.tryItem (index + 1) list |> Option.defaultValue current
        check index prev current next

// Задание 14 (1.40) Дан целочисленный массив. Необходимо найти минимальный четный элемент.
let findMinEven list =
    list
    |> List.filter (fun x -> x % 2 = 0)
    |> function
        | [] -> None
        | evens -> Some (List.min evens)

// Задание 15 (1.50) Для двух введенных списков L1 и L2 построить новый список, состоящий из элементов, встречающихся только в одном из этих списков и не повторяющихся в них.
let uniqueElements list1 list2 =
    let uniqueInL1 = list1 |> List.filter (fun x -> not (List.contains x list2))
    let uniqueInL2 = list2 |> List.filter (fun x -> not (List.contains x list1))
    List.append uniqueInL1 uniqueInL2

// Задание 16 (1.60) Дан список. Построить массив из элементов, делящихся на свой номер и встречающихся в исходном массиве 1 раз.
let buildSpecialArray list =
    list
    |> List.indexed
    |> List.filter (fun (i, x) -> 
        x % (i + 1) = 0 && 
        (List.filter ((=) x) list |> List.length) = 1
    )
    |> List.map snd



[<EntryPoint>]
let main _ =
    printChurchList churchList |> ignore
    printfn "\nList analysis:"
    printfn "Sum of even numbers: %d" (sumEvenNumbers churchList)
    printfn "Minimum element: %d" (findMin churchList)
    printfn "Count of odd numbers: %d" (countOdd churchList)
    printfn "Most frequent (recursive): %d" (findMostFrequent churchList)
    printfn "Most frequent (List module): %d" (findMostFrequentList churchList)
    printfn "Elements that are squares: %d" (countSquareElements churchList)
    
    printfn "\nBinary tree operations:"
    let numberTree = churchList |> listToTree
    printfn "Tree obhod:"
    printInOrder numberTree
    
    printfn "\nВведите количество строк:"
    let n = Console.ReadLine() |> int
    let stringList = readStringList n
    let sortedStrings = sortStringsByLength stringList
    printfn "\nОтсортированные строки по длине:"
    sortedStrings |> List.iter (printfn "%s")
    
    printfn "\nTriples result:"
    createTriples [5;3;7] [12;45;9] [6;8;15]
    |> List.iter (fun (x, y, z) -> printfn "(%d, %d, %d)" x y z)
    
    // Задание 11
    let list1 = [1;2;3;4]
    let list2 = [3;4;5;6]
    printfn "\nCommon elements count: %d" (countCommonElements list1 list2)

    // Задание 12
    let listWithGaps = [2;3;5;7]
    printfn "Missing numbers: %A" (findMissingNumbers listWithGaps)

    // Задание 13
    let testList = [1;3;2;4;5]
    printfn "Is index 1 local max: %b" (isLocalMax testList 1)

    // Задание 14
    let minEven = findMinEven [5;3;8;2;1]
    printfn "Min even element: %A" minEven

    // Задание 15
    let L1 = [1;2;3;4]
    let L2 = [3;4;5;6]
    printfn "Unique elements: %A" (uniqueElements L1 L2)

    // Задание 16
    let inputList = [2;4;6;3;12]
    printfn "Special array: %A" (buildSpecialArray inputList)
    
    0