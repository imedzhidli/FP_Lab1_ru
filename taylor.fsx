// Задание 1: Ряд Тейлора

open System
let buildinfunc x= (1. - float(Math.Pow(x, 2)/2.)) * float(cos(x)) - float(x)/2. * float(sin(x))

let a = 0.1
let b = 0.6
let n = 10
let eps = 0.0000000001

// Функция для вычисления факториала
let fact n =
    let rec fact' acc = function
    |   n when n <= 1  -> acc 
    |   n -> fact' (n*acc) (n-1)
    fact' 1 n

// Наивный Тейлор
let taylor_naive x = 
    let rec naive n acc term =
        if abs(term) < eps then acc, n
        else
            let term = (-1.) ** n * (2. * n ** 2. + 1.) * (x ** (2. * n)) / float(fact(2 * int n)) //i-тый член
            naive (n + 1.0) (acc + term) term
    naive 1 1. 1.


// Умный Тейлор
let taylor_smart x = 
    let rec taylor_0 n acc term = 
        if abs(term) < eps then acc, n
        else // Домножаем на высчитанную для этого ряда величину
          let term = term * ((-1.) * x**2. * (2. * float n ** 2. + 1.) * float(fact(2 * int n - 2)) / (float(fact(2*int(n))) * (2. * (n - 1.)**2. + 1.))) 
          taylor_0 (n + 1.0) (acc + term) term
    taylor_0 1 1. 1.

printfn "--------------------------------------------------------------------------------------"
printfn "|   x   |  Builtin  |     Smart Taylor     | # terms |     Dumb Taylor     | # terms |"
printfn "--------------------------------------------------------------------------------------"

let main =
   for i=0 to n do
    let x = a+(float i)/(float n)*(b-a)
    let smart, terms1 = taylor_smart x
    let naive, terms2 = taylor_naive x
    printfn "| %5.3f |  %-9.6f|       %-14.6f |    %.0f    |       %-14.6f|    %.0f    |" x (buildinfunc x) smart terms1 naive terms2


printfn "--------------------------------------------------------------------------------------"

main