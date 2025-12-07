open Base
open Stdio

let solve_part1 input =
  match input with
  | [] -> failwith ""
  | hd :: rest ->
    let start_idx, _ =
      hd |> String.findi ~f:(fun _ c -> Char.equal c 'S') |> Option.value_exn ~message:""
    in
    let _, num_split =
      rest
      |> List.fold_left ~init:([ start_idx ], 0) ~f:(fun (indices, num_split) line ->
        let new_splits =
          indices |> List.count ~f:(fun idx -> Char.equal (String.get line idx) '^')
        in
        let indices =
          indices
          |> List.map ~f:(fun idx ->
            match String.get line idx with
            | '^' -> [ idx - 1; idx + 1 ]
            | '.' -> [ idx ]
            | _ -> failwith "")
          |> List.concat
          |> List.remove_consecutive_duplicates ~equal:(fun a b -> a = b)
        in
        indices, num_split + new_splits)
    in
    num_split
;;

let rec merge_consecutives lst : (int * int) list =
  match lst with
  | [] -> []
  | a :: [] -> [ a ]
  | (a_idx, a_num) :: rest ->
    let rest = merge_consecutives rest in
    (match rest with
     | [] -> failwith ""
     | (b_idx, b_num) :: rest when a_idx = b_idx -> (a_idx, a_num + b_num) :: rest
     | (b_idx, b_num) :: rest -> (a_idx, a_num) :: (b_idx, b_num) :: rest)
;;

let solve_part2 input =
  match input with
  | [] -> failwith ""
  | hd :: rest ->
    let start_idx, _ =
      hd |> String.findi ~f:(fun _ c -> Char.equal c 'S') |> Option.value_exn ~message:""
    in
    rest
    |> List.fold_left
         ~init:[ start_idx, 1 ]
         ~f:(fun indices line ->
           indices
           |> List.map ~f:(fun (idx, num_paths) ->
             match String.get line idx with
             | '^' -> [ idx - 1, num_paths; idx + 1, num_paths ]
             | '.' -> [ idx, num_paths ]
             | _ -> failwith "")
           |> List.concat
           |> merge_consecutives)
    |> List.fold_left ~init:0 ~f:(fun acc (_, num) -> acc + num)
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
