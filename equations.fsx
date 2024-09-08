//Задание 2: Численное решение трансцендентных уравнений

open System
let eps = 0.0000000001

let rec dichotomy f (a: float) (b: float) (eps: float) = 
    let c = (a + b) / 2.
    if (abs (f c) >= eps) then
        if (f a * f c < 0) then
            dichotomy f a c eps
        else
            dichotomy f c b eps
    else 
        c

let rec iterations phi x0 eps = 
    let c = phi x0
    if (abs(x0 - c)) >= eps then
        iterations phi c eps
    else
        x0

let newthon f f' x0 eps =
    let phi x = x - (f x / f' x)
    iterations phi x0 eps

let f1 (x: float) = Math.Exp(x) + Math.Sqrt(1. + Math.Exp(2. * x)) - 2.
let f2 (x: float) = Math.Log(x) - x + 1.8
let f3 (x: float) = x * Math.Tan(x) - 1./3.

let f1' (x: float) = Math.Exp(x) + Math.Exp(x) / Math.Sqrt(1. + Math.Exp(2. * x))
let f2' (x: float) = 1. / x - 1.
let f3' (x: float) = Math.Tan(x) + x / Math.Cos(x)**2

let phi1 (x: float) = x - f1 x / f1' x 
let phi2 (x: float) = x - f2 x / f2' x 
let phi3 (x: float) = x - f3 x / f3' x 


printfn "----------------------------------------"
printfn "| № Eq | Dichotomy |  Iters  | Newthon |"
printfn "----------------------------------------"
let main = 
    printfn "|   1  |   %-8.4f| %7.4f | %7.4f |" (dichotomy f1 -1. 0. eps) (iterations phi1 -1. eps) (newthon f1 f1' -1. eps)
    printfn "|   2  |   %-8.4f| %7.4f | %7.4f |" (dichotomy f2 2. 3. eps) (iterations phi2 2. eps) (newthon f2 f2' 2. eps)
    printfn "|   3  |   %-8.4f| %7.4f | %7.4f |" (dichotomy f3 0.2 1. eps) (iterations phi3 0.2 eps) (newthon f3 f3' 0.2 eps)

printfn "----------------------------------------"
main