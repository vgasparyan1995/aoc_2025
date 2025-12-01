open Base
open Stdio

let abs_diff a b = abs (a - b)

let count_clicks a b =
  (abs_diff a b / 100)
  +
  if a < b
  then if b % 100 < a % 100 then 1 else 0
  else if a % 100 = 0
  then 0
  else if b % 100 = 0
  then 1
  else if a % 100 < b % 100
  then 1
  else 0
;;

let parse_input input =
  input
  |> List.map ~f:(fun line ->
    match String.to_list line with
    | 'L' :: rest -> -(rest |> String.of_char_list |> Int.of_string)
    | 'R' :: rest -> rest |> String.of_char_list |> Int.of_string
    | _ -> failwith "unexpected input")
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
    let count = count + count_clicks acc new_acc in
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
