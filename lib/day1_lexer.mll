{
  open Base
  type number = Word of int | Digit of int
}
rule token = parse
    ['0'-'9'] as d { Char.get_digit d |> Option.map ~f:(fun d -> Digit d) }
    | "zero" { Some (Word 0) }
    | "one" { Some (Word 1) }
    | "two" { Some (Word 2) }
    | "three" { Some (Word 3) }
    | "four" { Some (Word 4) }
    | "five" { Some (Word 5) }
    | "six" { Some (Word 6) }
    | "seven" { Some (Word 7) }
    | "eight" { Some (Word 8) }
    | "nine" { Some (Word 9) }
    | _ { token lexbuf }
    | eof { None }
  
