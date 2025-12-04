open Base
open Stdio

let parse_input input = input |> List.map ~f:String.to_array |> List.to_array

let cartesian_product a b =
  a |> List.map ~f:(fun i -> b |> List.map ~f:(fun j -> i, j)) |> List.concat
;;

let range a b = List.init (b - a) ~f:(fun i -> a + i)

let neighbors =
  cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
  |> List.filter ~f:(fun (i, j) -> not (i = 0 && j = 0))
;;

let get grid r c =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  if r = -1 || r = rows || c = -1 || c = cols then '.' else grid.(r).(c)
;;

let solve_part1 input =
  let grid = input |> parse_input in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  cartesian_product (range 0 rows) (range 0 cols)
  |> List.filter ~f:(fun (r, c) -> get grid r c |> Char.equal '@')
  |> List.map ~f:(fun (r, c) ->
    neighbors
    |> List.map ~f:(fun (dr, dc) -> r + dr, c + dc)
    |> List.map ~f:(fun (r, c) -> get grid r c)
    |> List.count ~f:(Char.equal '@'))
  |> List.count ~f:(fun num_neighbors -> num_neighbors < 4)
;;

let remove grid =
  let to_be_removed =
    let rows = Array.length grid in
    let cols = Array.length grid.(0) in
    cartesian_product (range 0 rows) (range 0 cols)
    |> List.filter ~f:(fun (r, c) -> get grid r c |> Char.equal '@')
    |> List.filter ~f:(fun (r, c) ->
      neighbors
      |> List.map ~f:(fun (dr, dc) -> r + dr, c + dc)
      |> List.map ~f:(fun (r, c) -> get grid r c)
      |> List.count ~f:(Char.equal '@')
      < 4)
  in
  let num_removed = to_be_removed |> List.length in
  let () = to_be_removed |> List.iter ~f:(fun (r, c) -> grid.(r).(c) <- '.') in
  grid, num_removed
;;

let rec remove_all grid n =
  let new_grid, num_removed = remove grid in
  if num_removed = 0 then n else remove_all new_grid (n + num_removed)
;;

let solve_part2 input =
  let grid = input |> parse_input in
  remove_all grid 0
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
