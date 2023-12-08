open! Imports
open! Base

module HandType = struct
  type t =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard
  [@@deriving show]

  let of_int_list x =
    match List.sort ~compare:Int.compare x with
    | [ 5 ] -> FiveOfAKind
    | [ 1; 4 ] -> FourOfAKind
    | [ 2; 3 ] -> FullHouse
    | [ 1; 1; 3 ] -> ThreeOfAKind
    | [ 1; 2; 2 ] -> TwoPair
    | [ 1; 1; 1; 2 ] -> OnePair
    | [ 1; 1; 1; 1; 1 ] -> HighCard
    | x ->
      failwith
        (String.concat
           ~sep:""
           ("Cannot covert this to HandType: " :: List.map ~f:Int.to_string x))
  ;;

  let to_int x =
    match x with
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1
  ;;

  let compare a b = Int.compare (to_int a) (to_int b)
end

module Hand = struct
  type t =
    { type_ : HandType.t
    ; cards : string
    ; bid : int
    }
  [@@deriving show]

  let of_string s =
    let cards, bid =
      match String.split ~on:' ' s with
      | [ c; b ] -> c, Int.of_string b
      | _ -> failwith (String.concat ~sep:"" [ "Cannot convert string to hand: "; s ])
    in
    let type_ =
      cards
      |> String.fold
           ~init:(Map.empty (module Char))
           ~f:(fun counts c ->
             let count =
               match Map.find counts c with
               | None -> 0
               | Some x -> x
             in
             Map.set counts ~key:c ~data:(count + 1))
      |> (fun m ->
           match Map.find m '*' with
           | None -> m
           | Some _ when Map.length m = 1 -> m
           | Some n_jokers ->
             (* Add jokers to the highest other count *)
             let new_counts = Map.remove m '*' in
             let key, v =
               new_counts
               |> Map.fold ~init:None ~f:(fun ~key ~data largest ->
                 match largest with
                 | None -> Some (key, data)
                 | Some (_, x) when x < data -> Some (key, data)
                 | x -> x)
               |> Option.value_exn
             in
             new_counts |> Map.set ~key ~data:(v + n_jokers))
      |> Map.data
      |> HandType.of_int_list
    in
    { type_; cards; bid }
  ;;

  let compare a b =
    let card_to_rank x =
      match x with
      | 'A' -> 14
      | 'K' -> 13
      | 'Q' -> 12
      | 'J' -> 11
      | 'T' -> 10
      | '9' -> 9
      | '8' -> 8
      | '7' -> 7
      | '6' -> 6
      | '5' -> 5
      | '4' -> 4
      | '3' -> 3
      | '2' -> 2
      | '*' -> 1
      | _ -> failwith ("Unrecognized card:" ^ String.make 1 x)
    in
    let compare_cards ca cb = Int.compare (card_to_rank ca) (card_to_rank cb) in
    match HandType.compare a.type_ b.type_ with
    | 0 ->
      Array.find
        ~f:(fun (ca, cb) -> not (Char.equal ca cb))
        (Array.zip_exn (String.to_array a.cards) (String.to_array b.cards))
      |> Option.map ~f:(fun (ca, cb) -> compare_cards ca cb)
      |> Option.value ~default:0
    | c -> c
  ;;
end

module M = struct
  type t = string list

  (* Parse the input to type t, invoked for both parts *)
  let parse input = input |> String.split_lines

  (* Run part 1 with parsed inputs *)
  let part1 lines =
    lines
    |> List.map ~f:Hand.of_string
    |> List.sort ~compare:Hand.compare
    |> List.foldi ~init:0 ~f:(fun i total (h : Hand.t) ->
      let rank = i + 1 in
      total + (rank * h.bid))
    |> Int.to_string
  ;;

  (* Run part 2 with parsed inputs *)
  let part2 lines =
    lines
    |> List.map ~f:(fun line ->
      line |> String.substr_replace_all ~pattern:"J" ~with_:"*" |> Hand.of_string)
    |> List.sort ~compare:Hand.compare
    |> List.foldi ~init:0 ~f:(fun i total (h : Hand.t) ->
      let rank = i + 1 in
      total + (rank * h.bid))
    |> Int.to_string
  ;;
end

include M
include Day.Make (M)

(* Example input *)
let example = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

(* Expect test for example input *)
let%expect_test _ =
  run example;
  [%expect {|
  Part 1: 6440
  Part 2: 5905
  |}]
;;
