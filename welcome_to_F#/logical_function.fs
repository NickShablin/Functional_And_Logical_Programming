let trickyFunction a =
    let func1 b = 
        let rec sumCifr item sum= 
            match item with
            |item when item > 0 -> sumCifr (item/10) (sum + item%10)
            | _ -> sum
                
        sumCifr b 0
    
    let func2 b = 
        let rec factorial item prod =
            match item with
            |item when item > 0 -> factorial (item-1) (prod*item)
            | _ -> prod
        factorial b 1

    match a with
    | true -> func1
    | false -> func2

System.Console.WriteLine(trickyFunction false 5)