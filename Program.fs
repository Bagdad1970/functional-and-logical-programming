open System

open WorkingWithNumbers.NumberOperations


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


[<EntryPoint>]
let main (args : string[]) =
    printfn "Hello, World"

(*
    printfn "Введите коэффициенты квадратного уравения a, b, c:"    
    let a = Double.Parse(Console.ReadLine())
    let b = Double.Parse(Console.ReadLine())
    let c = Double.Parse(Console.ReadLine())

    let roots = solve_quadr a b c
    match roots with
        None -> printfn "Нет решений"
        | Linear(x) -> printfn "Единственный корень: %f" x
        | Quadratic(x, y) -> printfn "Корни: %f %f" x y


    printfn "Введите радиус и высоту цилиндра:"
    let r = Double.Parse(Console.ReadLine())
    let h = Double.Parse(Console.ReadLine())
    
    let volume_superpos = volume_cylinder_through_superpos (r, h)
    printfn "(Суперпозиция) Объем цилиндра с радиусом основания %f и высотой %f: %f" r h volume_superpos

    let volume_carry = volume_cylinder_through_carry r h
    printfn "(Каррирование) Объем цилиндра с радиусом основания %f и высотой %f: %f" r h volume_carry
*)

    printfn "Введите число:"
    let num = System.Int32.Parse(Console.ReadLine())

    printfn "Сумма цифр числа: %d" (processDigits num 0 (+) )
    printfn "Прозведение цифр числа: %d" (processDigits num 1 (*) )

    0