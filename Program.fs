open System
open WorkingWithNumbers


type SolveQuadratic =
    None
    | Linear of float
    | Quadratic of float * float

let solve_quadr a b c =
        let D = b * b - 4. * a * c
        if a = 0. then
            if b = 0. then None
            else Linear(-c / b)
        else
            if D < 0. then None
            else Quadratic(( (-b + sqrt(D)) / (2. * a), (-b - sqrt(D)) / (2. * a) ))

let square_circle r = 
    (System.Math.PI * r ** 2.0)

let volume_cylinder_through_superpos (r, h) =
    let square_cylinder_base = square_circle r
    h * square_cylinder_base

let volume_cylinder_through_carry r h =
    let square_cylinder_base = square_circle r
    h * square_cylinder_base

let favouriteLanguage (lang: string) : unit =
    let result =
        match lang with
            "F#" | "Prolog" -> "НЕ ПОДЛИЗЫВАЙСЯ"
            | "Ruby" -> "Язык нормального человека"
            | "Java" -> "Язык курильщика"
            | _ -> "Это кто (who)?"

    result |> System.Console.WriteLine

let chooseMethod (func_num: int, num: int) : unit =
    match func_num with
    1 -> NumberOperations.countEvenNumsThatNotMutuallyPrimeWithNumber 1 num 0 |> Console.WriteLine
    | 2 -> NumberOperations.maxDigitNotDividesOn3 num |> Console.WriteLine
    | 3 -> NumberOperations.method3 num |> Console.WriteLine
    | _ -> Console.WriteLine("Неверный номер")


let introductionInvokers() =
    printfn "Hello, World"

    // Задание 2
    System.Console.WriteLine("Введите коэффициенты квадратного уравения a, b, c:")
    let a = Double.Parse(System.Console.ReadLine())
    let b = Double.Parse(System.Console.ReadLine())
    let c = Double.Parse(System.Console.ReadLine())

    let roots = solve_quadr a b c
    match roots with
        None -> System.Console.WriteLine("Нет решений")
        | Linear(x) -> System.Console.WriteLine("Единственный корень: {0}", x)
        | Quadratic(x, y) -> System.Console.WriteLine("Корни: {0} {1}", x, y)


    // Задание 3
    System.Console.WriteLine("Введите радиус и высоту цилиндра:")
    let r = Double.Parse(System.Console.ReadLine())
    let h = Double.Parse(System.Console.ReadLine())
    
    let volume_superpos = volume_cylinder_through_superpos (r, h)
    System.Console.WriteLine("(Суперпозиция) Объем цилиндра с радиусом основания {0} и высотой {1}: {2}", r, h, volume_superpos)

    let volume_carry = volume_cylinder_through_carry r h
    System.Console.WriteLine("(Каррирование) Объем цилиндра с радиусом основания {0} и высотой {1}: {2}", r, h, volume_carry)

    // Задание 4
    System.Console.WriteLine("Введите число:")
    let num = System.Int32.Parse(Console.ReadLine())

    System.Console.WriteLine("Рекурсия вверх")
    System.Console.WriteLine("Сумма цифр числа: {0}", (NumberOperations.processDigitsRecursionToTop num (+) ) )
    System.Console.WriteLine("Прозведение цифр числа: {0}", (NumberOperations.processDigitsRecursionToTop num (*) ) )

    // Задание 5
    System.Console.WriteLine("Рекурсия вниз")
    System.Console.WriteLine("Сумма цифр числа: {0}", (NumberOperations.processDigitsRecursionToDown (+) 0 num  ) )
    System.Console.WriteLine("Прозведение цифр числа: {0}", (NumberOperations.processDigitsRecursionToDown (*) 1 num ) )


    // Задание 6
    let factor = NumberOperations.chooseFunction false
    Console.WriteLine("Результат: {0}", (factor 1 6))
    Console.WriteLine("Результат: {0}", (factor 1 5))

    let factor1 = NumberOperations.chooseFunction true
    Console.WriteLine("Результат: {0}", (factor1 0 12345))
    Console.WriteLine("Результат: {0}", (factor1 0 1236781))

    // Задание 7-8
    let min_function = fun a b -> if a < b then a else b
    let min_digit = NumberOperations.bypassDigits 1234 min_function 10
    System.Console.WriteLine("Минимальная цифра числа: {0}", min_digit)

    let max_function = fun a b -> if a > b then a else b
    let max_digit = NumberOperations.bypassDigits 1234 max_function 0
    System.Console.WriteLine("Максимальная цифра числа: {0}", max_digit)

    let plus = fun a b -> a + b
    let plus_digits = NumberOperations.bypassDigits 1234 plus 0
    System.Console.WriteLine("Сумма цифр числа: {0}", plus_digits)

    let mult = fun a b -> a * b
    let mult_digits = NumberOperations.bypassDigits 1234 mult 1
    System.Console.WriteLine("Произведение цифр числа: {0}", mult_digits)


    // Задание 9 - 10
    let min_function = fun a b -> if a < b then a else b
    let evenCondition = fun a -> if a % 2 = 0 then true else false
    let min_digit = NumberOperations.bypassDigitsWithCondition 1234 min_function 10 evenCondition
    System.Console.WriteLine("Минимальная четная цифра числа: {0}", min_digit)

    let max_function = fun a b -> if a > b then a else b
    let oddCondition = fun a -> if a % 2 <> 0 then true else false
    let max_digit = NumberOperations.bypassDigitsWithCondition 1234 max_function 0 oddCondition
    System.Console.WriteLine("Максимальная нечетная цифра числа: {0}", max_digit)

    let plus = fun a b -> a + b
    let notOne = fun a -> if a <> 1 then true else false
    let plus_digits = NumberOperations.bypassDigitsWithCondition 1234 plus 0 notOne
    System.Console.WriteLine("Сумма цифр числа, которые не равны 1: {0}", plus_digits)

    let mult = fun a b -> a * b
    let notThree = fun a -> if a <> 3 then true else false
    let mult_digits = NumberOperations.bypassDigitsWithCondition 1234 mult 1 notThree
    System.Console.WriteLine("Произведение цифр числа, которые не равны 3: {0}", mult_digits)


    // Задание 11
    System.Console.Write("Какой Ваш любимый язык: ")
    let lang_choice = System.Console.ReadLine()
    favouriteLanguage lang_choice

    // Задание 16.2
    System.Console.WriteLine("Максимальная цифра, не делящаяся на 3: {0}", (NumberOperations.maxDigitNotDividesOn3 18404))

    // Задание 16.1
    let res = NumberOperations.countEvenNumsThatNotMutuallyPrimeWithNumber 1 20 0
    System.Console.WriteLine("{0}", res)


    // Задание 13
    let res = NumberOperations.bypassMutuallyPrimeComponentsInNumber 1 10 (fun a b -> a + b) 0
    System.Console.WriteLine("{0}", res)


    // Задание 14
    let num = Console.ReadLine()
    let res = NumberOperations.EulerFunction (int num)
    System.Console.WriteLine("Функция Эйлера от {0} есть {1}", num, res)


    // Задание 15
    let num = 15
    let res = NumberOperations.bypassMutuallyPrimeWithCondition 1 num (+) 0 (fun a -> a % 2 <> 0)
    System.Console.WriteLine("Сумма нечетных взаимно-простых чисел от 1 до {0} есть {1}", num, res)


    //Задание 16.3
    let num = Console.ReadLine() |> int
    let res = NumberOperations.method3 num
    System.Console.WriteLine("Метод 3: {0}", res)


    // Задание 20
    Console.Write("Выберите метод: ")
    let func_num = Console.ReadLine() |> int
    Console.Write("Введите число: ")
    let num = Console.ReadLine() |> int
    chooseMethod (func_num, num)

let listInvokers () =
(*
    // Задание 1
    System.Console.Write("Введите количество элементов списка: ")
    let n = System.Console.ReadLine() |> int
    let list = ListOperations.readList n
    printfn "Введенный список: %A" list

    // Задание 2
    System.Console.WriteLine("Вывод элементов списка: ")
    ListOperations.writeList list


    // Задание 3
    let reducedList = ListOperations.reduceListWithCondition [1; 6; 8; 10] (*) (fun a -> a % 2 = 0) 1
    System.Console.WriteLine("Свернутое значение списка: {0}", reducedList)

    // Задание 4
    let minElem = ListOperations.minInList [1; 5; 0; -2]
    System.Console.WriteLine("Минимальный элемент списка: {0}", minElem)

    let sumEven = ListOperations.sumEvenInList [1; 5; 0; -2]
    System.Console.WriteLine("Сумма четных элемент списка: {0}", sumEven)

    let countOdd = ListOperations.countOddInList [1; 5; 0; -2]
    System.Console.WriteLine("Количество нечетных элементов в списке: {0}", countOdd)


    // Задание 5
    let most_frequent = ListOperations.theMostFrequentInList [1; 5; 1; -6; 1; 0; 7; 3; 3]
    System.Console.WriteLine("Элемент {0} в списке повторяется наибольшее число раз: {1}", (fst most_frequent), (snd most_frequent))


*)

    let array = [| 1; 2; -2; 7; 23 |]

    // Задание 10.1
    System.Console.WriteLine("Индексы массива: {0}", sprintf "%A" (ListOperations.indexesOfDecreasingArray array))


    // Задание 10.2
    let a, b = 1, 10
    System.Console.WriteLine("Количество элементов в интервале {0} {1}: {2}", a, b, (ListOperations.countElemsInInterval array a b))

    // Задание 10.3
    System.Console.WriteLine("Два максимума в массиве: {0}", (ListOperations.twoMaxInArray array))

    // Задание 10.4
    let lb = 2
    let rb = 10
    System.Console.WriteLine("Элементы в промежутке от {0} до {1}: {2}", lb, rb, sprintf "%A" (ListOperations.elemsInRange array lb rb))

    // Задание 10.6
    let list = [ 5; 1; 7; 1; 2; 9; 6; 8; 3; 1; 8; 5; 1; 5; 2; 8; 8 ]
    let frequent_list = (ListOperations.elemFrequentsInList list Map.empty)
    printfn "Частоты списка: %A" frequent_list
    let elemsThreeTimes = ListOperations.elemFrequentsThreeTimes frequent_list
    printfn "Список элементов, встречающихся более 3 раз: %A" elemsThreeTimes

    


let stringInvokers () =
    
    // Задание 19
    System.Console.WriteLine("Строка {0} является палиндромом? {1}", "aboba", StringOperations.isPalindrom "aboba")
    System.Console.WriteLine("Строка {0} является палиндромом? {1}", "abobo", StringOperations.isPalindrom "abobo")


[<EntryPoint>]
let main (args : string[]) =

    listInvokers ()

    0