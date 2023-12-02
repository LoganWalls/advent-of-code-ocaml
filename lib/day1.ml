open! Imports
open! Base

module M = struct
  (* Type to parse the input into *)
  type matched_pattern = Digit of int | Alpha of int

  type t = ((int * int) * matched_pattern) list list

  (* Parse the input to type t, invoked for both parts *)
  let parse _inputs =
    let preprocess s =
      let patterns =
        [ ("1", Digit 1)
        ; ("2", Digit 2)
        ; ("3", Digit 3)
        ; ("4", Digit 4)
        ; ("5", Digit 5)
        ; ("6", Digit 6)
        ; ("7", Digit 7)
        ; ("8", Digit 8)
        ; ("9", Digit 9)
        ; ("one", Alpha 1)
        ; ("two", Alpha 2)
        ; ("three", Alpha 3)
        ; ("four", Alpha 4)
        ; ("five", Alpha 5)
        ; ("six", Alpha 6)
        ; ("seven", Alpha 7)
        ; ("eight", Alpha 8)
        ; ("nine", Alpha 9) ]
      and extrema inds : (int * int) option =
        Option.both
          (List.min_elt ~compare:min inds)
          (List.max_elt ~compare:max inds)
      in
      patterns
      |> List.filter_map ~f:(fun (p, v) ->
             String.substr_index_all ~may_overlap:false ~pattern:p s
             |> extrema
             |> Option.map ~f:(fun x -> (x, v)) )
    in
    String.split_lines _inputs |> List.map ~f:preprocess

  let reduce_extrema =
    let f (prev_low, prev_high) ((low, high), v) =
      let lowest_i, _ = prev_low and highest_i, _ = prev_high in
      let l = if low < lowest_i then (low, v) else prev_low
      and h = if high > highest_i then (high, v) else prev_high in
      (l, h)
    in
    List.fold ~init:((Int.max_value, 0), (Int.min_value, 0)) ~f

  let print_lists i =
    let print_items j =
      let print d = Stdio.printf "%d " d |> ignore in
      List.iter ~f:print j
    in
    List.iter ~f:print_items i ;
    Stdio.print_string "\n|\n" ;
    i

  (* Run part 1 with parsed inputs *)
  let part1 matches =
    let total =
      matches
      |> List.fold ~init:0 ~f:(fun total x ->
             let (_, first), (_, last) =
               x
               |> List.filter_map ~f:(fun (inds, v) ->
                      match v with Digit d -> Some (inds, d) | _ -> None )
               |> reduce_extrema
             in
             (first * 10) + last + total )
    in
    Stdio.printf "%d" total ; ()

  (* Run part 2 with parsed inputs *)
  let part2 matches =
    let total =
      matches
      |> List.fold ~init:0 ~f:(fun total x ->
             let (_, first), (_, last) =
               x
               |> List.filter_map ~f:(fun (inds, v) ->
                      match v with Digit d | Alpha d -> Some (inds, d) )
               |> reduce_extrema
             in
             (first * 10) + last + total )
    in
    Stdio.printf "%d" total ; ()
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
