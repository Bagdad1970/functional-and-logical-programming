namespace WorkingWithNumbers

module NumberOperations = 

    let rec processDigitsRecursionToDown (num: int) (accum: int) (oper: int -> int -> int)  = 
        if num = 0 then accum
        else
            let digit : int = num % 10
            let new_num : int = int num / 10
            let new_accum : int = oper digit accum
            processDigitsRecursionToDown new_num new_accum oper

    let rec processDigitsRecursionToTop (num: int) (oper: int -> int -> int)  = 
        if num < 10 then
            num
        else
            oper (processDigitsRecursionToTop (num / 10) oper) (num % 10)