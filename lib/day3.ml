open! Imports
open! Base

module M = struct
  (* Type to parse the input into *)
  type part_number =
    { left : int
    ; right : int
    ; value : int
    }
  [@@deriving show]

  type symbol =
    { i : int
    ; value : char
    }
  [@@deriving show]

  let print_part p = show_part_number p |> Stdio.print_endline

  type t = (symbol list * part_number list) list [@@deriving show]

  (* Parse the input to type t, invoked for both parts *)
  let parse inputs =
    inputs
    |> String.split_lines
    |> List.map ~f:(fun line ->
      let symbols =
        line
        |> String.to_list
        |> List.filter_mapi ~f:(fun i c ->
          if not (Char.is_digit c || Char.equal c '.')
          then Some { i; value = c }
          else None)
      and part_nums =
        line
        |> Re2.get_matches_exn (Re2.create_exn "([0-9]+)")
        |> List.map ~f:(fun m ->
          let start, len = Re2.Match.get_pos_exn ~sub:(`Index 1) m
          and value = m |> Re2.Match.get_exn ~sub:(`Index 1) |> Int.of_string in
          { left = start - 1; right = start + len; value })
      in
      symbols, part_nums)
  ;;

  (* Run part 1 with parsed inputs *)
  let part1 rows =
    rows
    |> List.foldi ~init:0 ~f:(fun i total (symbols, part_nums) ->
      let prev_symbols = List.nth rows (i - 1) |> Option.map ~f:(fun (s, _) -> s)
      and next_symbols = List.nth rows (i + 1) |> Option.map ~f:(fun (s, _) -> s) in
      let all_symbols =
        [ prev_symbols; Some symbols; next_symbols ]
        |> List.filter_opt
        |> List.concat
        |> List.map ~f:(fun s -> s.i)
      in
      total
      + (part_nums
         |> List.fold ~init:0 ~f:(fun subtotal p ->
           if List.exists ~f:(Int.between ~low:p.left ~high:p.right) all_symbols
           then subtotal + p.value
           else subtotal)))
    |> Int.to_string
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 rows =
    rows
    |> List.foldi ~init:0 ~f:(fun i total (symbols, part_nums) ->
      let prev_parts = List.nth rows (i - 1) |> Option.map ~f:(fun (_, p) -> p)
      and next_parts = List.nth rows (i + 1) |> Option.map ~f:(fun (_, p) -> p) in
      let all_parts =
        [ prev_parts; Some part_nums; next_parts ] |> List.filter_opt |> List.concat
      and gear_candidates =
        symbols
        |> List.filter_map ~f:(fun s -> if Char.equal '*' s.value then Some s.i else None)
      in
      total
      + (gear_candidates
         |> List.fold ~init:0 ~f:(fun subtotal candidate ->
           let adjacent_parts =
             List.filter_map
               ~f:(fun p ->
                 if Int.between ~low:p.left ~high:p.right candidate
                 then Some p.value
                 else None)
               all_parts
           in
           if List.length adjacent_parts = 2
           then (List.nth_exn adjacent_parts 0 * List.nth_exn adjacent_parts 1) + subtotal
           else subtotal)))
    |> Int.to_string
  ;;
end

include M
include Day.Make (M)

(* Example input *)
let example =
  "467..114..\n\
   ...*......\n\
   ..35..633.\n\
   ......#...\n\
   617*......\n\
   .....+.58.\n\
   ..592.....\n\
   ......755.\n\
   ...$.*....\n\
   .664.598.."
;;

(* Expect test for example input *)
let%expect_test _ =
  run example;
  [%expect {|
  Part 1: 4361
  Part 2: 467835
  |}]
;;
