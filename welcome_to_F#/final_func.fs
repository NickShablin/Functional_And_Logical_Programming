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