open! Imports
open! Base

module M = struct
  (* Type to parse the input into *)
  type t = Day1_lexer.number list list

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let tokenize lexbuf =
      let rec next_token tokens =
        match Day1_lexer.token lexbuf with
        | Some t -> next_token (t :: tokens)
        | None -> tokens
      in
      next_token [] |> List.rev
    in
    String.split_lines _inputs
    |> List.map ~f:(fun s -> Lexing.from_string s |> tokenize)

  let print_total lines =
    let digit_sum l = (List.hd_exn l * 10) + List.last_exn l in
    lines
    |> List.fold ~init:0 ~f:(fun a c -> a + digit_sum c)
    |> Stdio.printf "%d\n" |> ignore

  let print_list i =
    let print_items j =
      let print d = Stdio.printf "%d  " d |> ignore in
      List.iter ~f:print j
    in
    let () = List.iter ~f:print_items i
    and _ = Stdio.print_string "\n|\n\n" in
    i

  (* Run part 1 with parsed inputs *)
  let part1 lines =
    lines
    |> List.map
         ~f:
           (List.filter_map ~f:(fun n ->
                match n with Day1_lexer.Digit d -> Some d | _ -> None ) )
    |> print_total

  (* Run part 2 with parsed inputs *)
  let part2 lines =
    lines
    |> List.map
         ~f:
           (List.map ~f:(fun n ->
                match n with Day1_lexer.Digit d | Day1_lexer.Word d -> d ) )
    |> print_total
end

include M
include Day.Make (M)

(* Expect test for example input *)
let%expect_test _ =
  run "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet\n" ~only_part1:true ;
  [%expect {| Part 1: 142 |}]

let%expect_test _ =
  run
    "two1nine\n\
    \ eightwothree\n\
    \ abcone2threexyz\n\
    \ xtwone3four\n\
    \ 4nineeightseven2\n\
    \ zoneight234\n\
    \ 7pqrstsixteen" ~only_part2:true ;
  [%expect {| Part 2: 281 |}]
