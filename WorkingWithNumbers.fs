namespace WorkingWithNumbers
open System


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

    let rec countEvenNumsThatNotMutuallyPrimeWithNumber (current: int) (num: int) (accum: int)  =
        match current with
        x when x >= num -> accum
        | x when GCD(num, x) <> 1 && x % 2 = 0 -> countEvenNumsThatNotMutuallyPrimeWithNumber (current + 1) num (accum + 1)
        | _ -> countEvenNumsThatNotMutuallyPrimeWithNumber (current + 1) num accum

    let maxDigitNotDividesOn3 (num: int) : int =
        bypassDigitsWithCondition num (fun a b -> if a > b then a else b) 0 (fun a -> if a % 3 <> 0 then true else false)

    let rec bypassMutuallyPrimeComponentsInNumber (current: int) (num: int) (func: int -> int -> int) (accum: int) : int =
        match current with
        x when x >= num -> accum
        | x when GCD(num, x) = 1 -> bypassMutuallyPrimeComponentsInNumber (current+1) num func (func current accum)
        | _ -> bypassMutuallyPrimeComponentsInNumber (current + 1) num func accum


    let EulerFunction (num: int) : int =
        bypassMutuallyPrimeComponentsInNumber 1 num (fun x acc -> acc + 1) 0

    let rec bypassMutuallyPrimeWithCondition (current: int) (num: int) (func: int -> int -> int) (accum: int) (condition: int -> bool) =
        match current with
        x when x >= num -> accum
        | x when GCD (num, x) = 1 && condition x -> 
            bypassMutuallyPrimeWithCondition (current + 1) num func (func accum current) condition
        | _ -> bypassMutuallyPrimeWithCondition (current + 1) num func accum condition
    let rec bypassNotMutuallyPrimeWithCondition (current: int) (num: int) (func: int -> int -> int) (accum: int) (condition: int -> bool) =
        match current with
        x when x >= num -> accum
        | x when GCD (num, x) <> 1 && condition x -> 
            bypassNotMutuallyPrimeWithCondition (current + 1) num func (func accum current) condition
        | _ -> bypassNotMutuallyPrimeWithCondition (current + 1) num func accum condition

    let rec smallestDivisor (current: int) (num: int) (accum: int) : int =
        match current with
        x when x >= num -> accum
        | x when num % x = 0 && x < accum -> smallestDivisor (current + 1) num x
        | _ -> smallestDivisor (current + 1) num accum

    let findMaxNonCoprime (num: int) : int =
        let smallest_div= smallestDivisor 2 num 10
        bypassNotMutuallyPrimeWithCondition 1 num (fun acc x -> max acc x) 0 (fun x -> x % smallest_div <> 0)

    let method3 (num: int) =
        let maxNonCoprime = findMaxNonCoprime num
        let digitSum = bypassDigitsWithCondition num (+) 0 (fun x -> x < 5)
        maxNonCoprime * digitSum


module ListOperations = 
    let readList (n: int) =
        let rec readNumbers remaining accum =
            match remaining with
            | 0 -> List.rev accum
            | x when x > 0 ->
                let newElem = System.Console.ReadLine() |> int
                readNumbers (remaining - 1) (newElem :: accum)
            | _ -> failwith "Ошибка в рекурсивной функции"

        match n with 
        | x when x > 0 -> readNumbers n []
        | x when x < 0 -> failwith "Количество элементов не может быть отрицательным"
        | 0 -> []
        | _ -> failwith "Непредвиденная ошибка"

    let rec writeList list =
        match list with
        | [] -> ()
        | head :: tail -> 
            System.Console.WriteLine(head.ToString())
            writeList tail

    let rec reduceListWithCondition list (func: int -> int -> int) (condition: int -> bool) (accum: int) =
        match list with
        [] -> accum
        | head :: tail when condition head -> reduceListWithCondition tail func condition (func accum head)
        | head :: tail when (condition head) = false  -> reduceListWithCondition tail func condition accum
        | _ -> failwith "Непредвиденная ошибка"

    let sumEvenInList list =
        reduceListWithCondition list (+) (fun a -> a % 2 = 0) 0

    let countOddInList list =
        reduceListWithCondition list (fun a _ -> a + 1) (fun a -> a % 2 <> 0) 0

    let minInList list =
        reduceListWithCondition list min (fun a -> true) 10


    let rec elemFrequentsInList list accum_map = 
        match list with
        [] -> accum_map
        | head :: tail -> 
            if Map.containsKey head accum_map then
                let currentCount = accum_map.[head]
                elemFrequentsInList tail (Map.add head (currentCount + 1) accum_map)
            else
                elemFrequentsInList tail (Map.add head 1 accum_map)


    let theMostFrequentInList list =
        let frequencyMap = elemFrequentsInList list Map.empty

        let mostFrequent = 
            frequencyMap |> Map.toSeq |> Seq.maxBy snd
        
        mostFrequent

    let indexesOfDecreasingArray (array: int[]) =
        array
        |> Array.indexed
        |> Array.sortByDescending snd
        |> Array.map fst

    let countElemsInInterval (array: int[]) (a: int) (b: int) : int =
        let start_index = max a 0
        let end_index = min b (array.Length - 1)

        end_index - start_index + 1

    let twoMaxInArray (array: int[]) =
        let first_max = Array.max array
        let second_max = Array.max (Array.filter (fun a -> a <> first_max) array)

        (first_max, second_max)

    let elemsInRange (array: int[]) lb rb =
        Array.filter (fun a -> a >= lb && a <= rb) array

    let elemFrequentsThreeTimes map =
        Map.filter (fun _ value -> value > 3) map

module StringOperations =
    let isPalindrom (str: string) =
        let reversedString = str.ToLower() |> Seq.rev |> System.String.Concat
        String.Equals (str, reversedString)