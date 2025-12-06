open Base
open Stdio

let solve_part1 input =
  let rows =
    input
    |> List.map ~f:(fun line ->
      line
      |> String.split_on_chars ~on:[ ' ' ]
      |> List.filter ~f:(fun s -> not (String.is_empty s)))
  in
  match rows |> List.transpose with
  | None -> failwith "bad input"
  | Some columns ->
    columns
    |> List.map ~f:(fun col ->
      match col |> List.rev with
      | "*" :: rest ->
        rest |> List.map ~f:Int.of_string |> List.fold_left ~init:1 ~f:( * )
      | "+" :: rest ->
        rest |> List.map ~f:Int.of_string |> List.fold_left ~init:0 ~f:( + )
      | _ -> failwith "bad input")
    |> List.fold_left ~init:0 ~f:( + )
;;

let solve_part2 input =
  let grid =
    input
    |> List.map ~f:String.to_list
    |> List.transpose
    |> Option.value_exn ~message:"Bad input"
  in
  grid
  |> List.map ~f:(fun col -> col |> List.filter ~f:(fun c -> not (Char.equal ' ' c)))
  |> List.map ~f:String.of_list
  |> List.group ~break:(fun _ r -> String.is_empty r)
  |> List.map ~f:(fun group ->
    match group with
    | "" :: rest -> rest
    | group -> group)
  |> List.map ~f:(fun group ->
    match group with
    | head :: rest ->
      let len = String.length head in
      let op = String.get head (len - 1) in
      let num = String.sub head ~pos:0 ~len:(len - 1) in
      let numbers = num :: rest in
      let numbers = numbers |> List.map ~f:Int.of_string in
      op, numbers
    | _ -> failwith "Empty group?")
  |> List.map ~f:(fun (op, numbers) ->
    match op with
    | '+' -> numbers |> List.fold_left ~init:0 ~f:( + )
    | '*' -> numbers |> List.fold_left ~init:1 ~f:( * )
    | _ -> failwith "Unexpected op")
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
