module type S = sig
  val run : ?only_part1:bool -> ?only_part2:bool -> string -> unit
end

module type Impl = sig
  type t

  val parse : string -> t
  val part1 : t -> string
  val part2 : t -> string
end

module Make (Impl : Impl) : S = struct
  let run ?(only_part1 = false) ?(only_part2 = false) inputs =
    let parsed = Impl.parse inputs in
    let () =
      if not only_part2
      then (
        let () = print_string "Part 1: " in
        Impl.part1 parsed |> Stdio.print_endline)
    in
    let () =
      if not only_part1
      then (
        let () = print_string "Part 2: " in
        Impl.part2 parsed |> Stdio.print_endline)
    in
    ()
  ;;
end
