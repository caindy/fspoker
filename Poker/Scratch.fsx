let (|Prime|_|) i =
  if i = 1 then None
  else
    let test = Seq.tryFind (fun x -> i % x = 0)
    let divisors = [2..i - 1]
    match test divisors with
    | None -> Some i
    | _ -> None

let (|FizzBuzz|_|) f i = 
  match i with
  | x when x % f = 0 -> Some i
  | _ -> None

let ``Fizz Buzz`` = 
  function | 3 -> "Fizz Prime!"
           | 5 -> "Buzz Prime!"
           | FizzBuzz 3 _ & FizzBuzz 5 _ -> "Fizz Buzz"
           | FizzBuzz 3 _ -> "Fizz"
           | FizzBuzz 5 _ -> "Buzz"
           | Prime _ -> "Prime!"
           | x  -> x.ToString() 
let output = ``Fizz Buzz`` >> (printfn "%s") 
[|1..100|] |> Array.map output

let rec fib n a b =
  match n with
  | 0 -> b
  | _ -> fib (n-1) b (a+b)
let fibonacci n = fib n 1I 0I
let time f =
  let stopwatch = System.Diagnostics.Stopwatch()
  stopwatch.Start()
  let i = f()
  stopwatch.Stop()
  stopwatch.ElapsedMilliseconds, i

let benchmark () =
  seq { 
    let x = ref 1
    let t = ref 0L
    while t < ref 100L do
      let (t', i) = time (fun () -> fibonacci !x)
      t := t'
      x := !x + 1
      yield (!t, i)
  }  |> Seq.last |> (fun (t,i) -> printfn "took %i for %A" t i)
time benchmark |> (fun (t,_) -> printfn "total elapsed: %i" t)

type Foo =
  | F of int
let g (p:Quotations.Expr<int -> string>) x = <@@ let i = (%%p x) in i @@>
<@ F @>
