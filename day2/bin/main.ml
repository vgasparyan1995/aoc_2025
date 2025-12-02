open Base
open Stdio
open Printf

let parse_input input =
  match input with
  | [ line ] ->
    line
    |> String.split_on_chars ~on:[ ',' ]
    |> List.map ~f:(fun range ->
      match range |> String.split_on_chars ~on:[ '-' ] with
      | [ rbegin; rend ] -> Int.of_string rbegin, Int.of_string rend
      | _ -> failwith (sprintf "unexpected input: %s" range))
  | _ -> failwith "unexpected input"
;;

let split_num n =
  let n_str = Int.to_string n in
  let len = String.length n_str in
  if len % 2 <> 0
  then None
  else (
    let n1 = String.sub n_str ~pos:0 ~len:(len / 2) in
    let n1 = if String.is_empty n1 then 0 else Int.of_string n1 in
    let n2 = String.sub n_str ~pos:(len / 2) ~len:(len - (len / 2)) in
    let n2 = Int.of_string n2 in
    Some (n1, n2))
;;

let create_range a b = List.init (b - a) ~f:(fun i -> a + i)

let solve_part1 input =
  input
  |> parse_input
  |> List.map ~f:(fun (a, b) -> create_range a (b + 1))
  |> List.concat
  |> List.filter ~f:(fun n ->
    match split_num n with
    | Some (a, b) -> a = b
    | None -> false)
  |> List.fold ~f:( + ) ~init:0
;;

let rec all_same_elements lst =
  match lst with
  | [] -> true
  | [ _ ] -> true
  | a :: b :: rest -> Poly.(a = b) && all_same_elements (b :: rest)
;;

let is_repetetive str =
  let str_len = String.length str in
  create_range 1 (str_len)
  |> List.map ~f:(fun i ->
    let tmp =
      str |> String.to_list |> List.chunks_of ~length:i |> List.map ~f:String.of_char_list
    in
    tmp |> all_same_elements)
  |> List.exists ~f:(fun item -> item)
;;

let solve_part2 input =
  input
  |> parse_input
  |> List.map ~f:(fun (a, b) -> create_range a (b + 1))
  |> List.concat
  |> List.filter ~f:(fun n ->
    let n_str = Int.to_string n in
    is_repetetive n_str)
  |> List.fold ~f:( + ) ~init:0
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
