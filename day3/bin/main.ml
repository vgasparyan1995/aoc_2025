open Base
open Stdio

let parse_input (input : string list) : int list list =
  input
  |> List.map ~f:(fun line ->
    line |> String.to_list |> List.map ~f:(fun d -> d |> Char.to_string |> Int.of_string))
;;

let extract_max_digit (l : (int * int) list)
  : (int * int) list * (int * int) * (int * int) list
  =
  let max_item =
    l
    |> List.fold_left ~init:(-1, 0) ~f:(fun (max_i, max_d) (i, d) ->
      if d > max_d then i, d else max_i, max_d)
  in
  let left, right = l |> List.split_while ~f:(fun item -> Poly.(item <> max_item)) in
  let right = List.drop right 1 in
  left, max_item, right
;;

let rec extract_digits (n : int) (lists : (int * int) list list) : (int * int) list =
  match n with
  | 0 -> []
  | n ->
    (match lists with
     | hd :: rest ->
       let left, item, right = extract_max_digit hd in
       let lists =
         match left, right with
         | [], [] -> rest
         | left, [] -> left :: rest
         | [], right -> right :: rest
         | left, right -> right :: left :: rest
       in
       item :: extract_digits (n - 1) lists
     | _ -> failwith "unexpected")
;;

let solve_part1 input =
  input
  |> parse_input
  |> List.map ~f:(fun digits ->
    let digits : (int * int) list = digits |> List.mapi ~f:(fun i d -> i, d) in
    [ digits ]
    |> extract_digits 2
    |> List.sort ~compare:(fun (ai, _) (bi, _) -> ai - bi)
    |> List.map ~f:(fun (_, d) -> d)
    |> List.fold_left ~init:0 ~f:(fun acc d -> (acc * 10) + d))
  |> List.map ~f:(fun n -> n)
  |> List.fold_left ~init:0 ~f:( + )
;;

let solve_part2 input =
  input
  |> parse_input
  |> List.map ~f:(fun digits ->
    let digits : (int * int) list = digits |> List.mapi ~f:(fun i d -> i, d) in
    [ digits ]
    |> extract_digits 12
    |> List.sort ~compare:(fun (ai, _) (bi, _) -> ai - bi)
    |> List.map ~f:(fun (_, d) -> d)
    |> List.fold_left ~init:0 ~f:(fun acc d -> (acc * 10) + d))
  |> List.map ~f:(fun n -> n)
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
