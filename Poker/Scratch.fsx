let (|Fizz|_|) i =
  if i % 3 = 0 then Some i
  else None

let (|Buzz|_|) i =
  if i % 5 = 0 then Some i
  else None

let (|``Fizz Buzz``|_|) i =
  match i with
  | Fizz _ & Buzz _ -> Some "Fizz Buzz"
  | Fizz i -> Some "Fizz"
  | Buzz i -> Some "Buzz"
  | _ -> None

for i in [1..100] do
  match i with
  |``Fizz Buzz`` fb -> System.Console.WriteLine fb
  | _               -> System.Console.WriteLine "nothing"
