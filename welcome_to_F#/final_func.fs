let finalFunc digit funct init funcItsNeed=
    let rec step digit res=
        let itsNeed = funcItsNeed (digit%10)
        let next_digit = digit/10

        let next_res = funct res (digit%10) 
       
        match (next_digit, itsNeed) with
        | (next_digit,true) when next_digit > 0 -> step next_digit next_res
        | (next_digit,false) when next_digit > 0 -> step next_digit res 
        | (next_digit,true) -> next_res
        | _ -> res

    step digit init


System.Console.WriteLine(finalFunc 92722 (fun x y -> x+y) 0 (fun x -> match x%2 with |1 -> true |0->false))
System.Console.WriteLine(finalFunc 848 (fun x y -> x*y) 1 (fun x -> true))
System.Console.WriteLine(finalFunc 777 (fun x y -> match x>y with| true -> x| false -> y) 0 (fun x -> match x with |x when x>5 -> true|_ -> false))
System.Console.WriteLine(finalFunc 911 (fun x y -> match x<y with| true -> x| false -> y) 9 (fun x -> false))
