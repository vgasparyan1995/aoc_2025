open Base
open Stdio

let parse_input input =
  let ranges, values =
    input |> List.split_while ~f:(fun line -> not (String.is_empty line))
  in
  let ranges =
    ranges
    |> List.map ~f:(fun range ->
      match range |> String.split_on_chars ~on:[ '-' ] with
      | [ a; b ] -> Int.of_string a, Int.of_string b
      | _ -> failwith "bad input")
  in
  let values = List.drop values 1 in
  let values = values |> List.map ~f:Int.of_string in
  ranges, values
;;

let solve_part1 input =
  let ranges, values = input |> parse_input in
  values
  |> List.count ~f:(fun v ->
    ranges
    |> List.exists ~f:(fun range ->
      let a, b = range in
      v >= a && v <= b))
;;

let add_range ranges new_range =
  let intersecting, non_intersecting =
    ranges
    |> List.partition_tf ~f:(fun range ->
      let a, b = range in
      let na, nb = new_range in
      (a <= na && b >= na)
      || (a <= nb && b >= nb)
      || (a >= na && b <= nb)
      || (na >= a && nb <= b))
  in
  let new_range =
    intersecting
    |> List.fold_left ~init:new_range ~f:(fun (acc_a, acc_b) (a, b) ->
      min acc_a a, max acc_b b)
  in
  new_range :: non_intersecting
;;

let solve_part2 input =
  let ranges, _ = input |> parse_input in
  ranges
  |> List.fold_left ~init:[] ~f:(fun acc range -> add_range acc range)
  |> List.map ~f:(fun (a, b) -> b - a + 1)
  |> List.fold_left ~init:0 ~f:( + )
;;

let sample_input = "sample.txt" |> In_channel.read_lines
let input = "input.txt" |> In_channel.read_lines
let _ = print_endline "================"
let _ = print_endline "==   Part 1   =="
let _ = print_endline "================"
let _ = printf "%d\n" (solve_part1 sample_input)
let _ = printf "%d\n" (solve_part1 input)
let _ = print_endline "================"
let _ = print_endline "==   Part 2   =="
let _ = print_endline "================"
let _ = printf "%d\n" (solve_part2 sample_input)
let _ = printf "%d\n" (solve_part2 input)
