namespace WorkingWithNumbers

module NumberOperations = 

    let rec processDigitsRecursionToDown (oper: int -> int -> int) (accum: int) (num: int)   = 
        if num < 10 then
            oper accum num
        else
            processDigitsRecursionToDown oper (oper accum (num % 10)) (num / 10) 

    let rec processDigitsRecursionToTop (num: int) (oper: int -> int -> int)  = 
        if num < 10 then
            num
        else
            oper (processDigitsRecursionToTop (num / 10) oper) (num % 10)

    let rec factorialRecursionToTop (num: int) =
        if num <= 1 then
            1
        else
            num * factorialRecursionToTop(num - 1)

    let rec factorialRecursionToDown (accum: int) (num: int) =
        if num <= 1 then
            accum
        else
            factorialRecursionToDown (accum * num) (num - 1)

    let chooseFunction (digitSum: bool) =
        match digitSum with
        true -> processDigitsRecursionToDown (+)
        | false -> factorialRecursionToDown

    let rec bypassDigits (num: int) (func: int -> int -> int) (accum: int) : int =
        match num with
        0 -> accum
        | _ -> bypassDigits (int num / 10) func (func (num % 10) accum) 

    let rec bypassDigitsWithCondition (num: int) (twoArgFunc: int -> int -> int) (accum: int) (condition: int -> bool) : int =
        match num with
        0 -> accum
        | _ when (condition (num % 10)) = true -> bypassDigitsWithCondition (num / 10) twoArgFunc (twoArgFunc (num % 10) accum) condition
        | _ -> bypassDigitsWithCondition (num / 10) twoArgFunc accum condition

    let rec GCD (a: int, b: int) : int =
        match b with
        0 -> a
        | _ -> GCD (b, a % b)



    //let rec maxSimpleDivider (num: int) (counter: int) : int =
    //    match num with
    //    0 -> counter
    //    | GCD(num, )

    //let rec bypassMutuallyPrimeComponentsInNum (num: int) (func: int -> int -> int) (accum: int) : int =
    //    match num with
    //    0 -> accum
    //    | _ -> bypassMutuallyPrimeComponentsInNum (num - 1) func accum

    let maxDigitNotDividesOn3 (num: int) : int =
        bypassDigitsWithCondition num (fun a b -> if a > b then a else b) 0 (fun a -> if a % 3 <> 0 then true else false)