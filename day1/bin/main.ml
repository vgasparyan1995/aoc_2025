open Base
open Stdio

let abs_diff a b = abs (a - b)
let window a = (a - (a % 100)) / 100

let parse_input input =
  input
  |> List.map ~f:(fun line ->
    let len = String.length line in
    let first = String.get line 0 in
    let n = Int.of_string (String.sub line ~pos:1 ~len:(len - 1)) in
    let value =
      match first with
      | 'L' -> -n
      | 'R' -> n
      | _ -> failwith "unexpected input"
    in
    value)
;;

let solve_part1 input =
  input
  |> parse_input
  |> List.fold ~init:(50, 0) ~f:(fun (acc, count) x ->
    let acc = acc + x in
    let count = if acc % 100 = 0 then count + 1 else count in
    acc, count)
  |> (fun (_, count) -> count)
  |> Int.to_string
  |> print_endline
;;

let solve_part2 input =
  input
  |> parse_input
  |> List.fold ~init:(50, 0) ~f:(fun (acc, count) x ->
    let new_acc = acc + x in
    let num_wraparound = abs_diff (window new_acc) (window acc) in
    let edge_case_1 = if new_acc % 100 = 0 && x < 0 then 1 else 0 in
    let edge_case_2 = if acc % 100 = 0 && x < 0 then -1 else 0 in
    let count = count + num_wraparound + edge_case_1 + edge_case_2 in
    new_acc, count)
  |> (fun (_, count) -> count)
  |> Int.to_string
  |> print_endline
;;

let sample_input = "sample.txt" |> In_channel.read_lines
let input = "input.txt" |> In_channel.read_lines
let () = print_endline "================"
let () = print_endline "==   Part 1   =="
let () = print_endline "================"
let () = solve_part1 sample_input
let () = solve_part1 input
let () = print_endline "================"
let () = print_endline "==   Part 2   =="
let () = print_endline "================"
let () = solve_part2 sample_input
let () = solve_part2 input
