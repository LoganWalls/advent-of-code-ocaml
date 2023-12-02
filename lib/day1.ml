open! Imports
open! Base

module M = struct
  (* Type to parse the input into *)
  type matched_pattern =
    | Digit of {i: int; value: int}
    | Alpha of {i: int; value: int}
  let matched_i m = match m with Digit {i; _} -> i | Alpha {i; _} -> i
  let matched_value m = match m with Digit { value; _} -> value | Alpha { value; _} -> value

  type t = matched_pattern list list

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let preprocess s =
      let digit_matches =
        s |> String.to_list
        |> List.filter_mapi ~f:(fun i c ->
               c |> Char.get_digit
               |> Option.map ~f:(fun value -> Digit {i; value}) )
      and alpha_matches =
        let patterns =
          [ ("one", 1)
          ; ("two", 2)
          ; ("three", 3)
          ; ("four", 4)
          ; ("five", 5)
          ; ("six", 6)
          ; ("seven", 7)
          ; ("eight", 8)
          ; ("nine", 9) ]
        in
        patterns
        |> List.map ~f:(fun (pattern, value) ->
               String.substr_index_all ~may_overlap:false ~pattern s
               |> List.map ~f:(fun i -> Alpha {i; value}) )
        |> List.concat
      in
      digit_matches @ alpha_matches
    in
    String.split_lines _inputs |> List.map ~f:preprocess

  let reduce_extrema =
    let f (low, high) v =
      let l = if matched_i v < matched_i low then v else low
      and h = if matched_i v > matched_i high then v else high in
      (l, h)
    in
    List.fold ~init:(Digit {i = Int.max_value; value = 0}, Digit {i = Int.min_value; value = 0}) ~f

  (* Run part 1 with parsed inputs *)
  let part1 matches =
    let total =
      matches
      |> List.fold ~init:0 ~f:(fun total x ->
             let (first, last) =
               x
          |> List.filter ~f:(fun v -> match v with Digit _ -> true | _ -> false)
               |> reduce_extrema
             in
             (* Stdio.printf "%d + %d\n" (matched_value first) (matched_value last) ; *)
             matched_value first * 10 + matched_value last + total )
    in
    Stdio.printf "%d" total ; ()

  (* Run part 2 with parsed inputs *)
  let part2 matches =
    let total =
      matches
      |> List.fold ~init:0 ~f:(fun total x ->
             let (first, last) = reduce_extrema x
             in
             (* Stdio.printf "%d + %d\n" (matched_value first) (matched_value last) ; *)
             matched_value first * 10 + matched_value last + total )
    in
    Stdio.printf "%d\n" total ; ()
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
