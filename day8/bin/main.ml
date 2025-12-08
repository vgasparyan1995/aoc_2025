open Base
open Core
open Stdio

type point = int * int * int

module Point = struct
  module T = struct
    type t = point

    let compare x y =
      Tuple3.compare ~cmp1:Int.compare ~cmp2:Int.compare ~cmp3:Int.compare x y
    ;;

    let sexp_of_t = Tuple3.sexp_of_t Int.sexp_of_t Int.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Tuple3.t_of_sexp Int.t_of_sexp Int.t_of_sexp Int.t_of_sexp
    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
end

let cartesian_product a b =
  a |> List.map ~f:(fun i -> b |> List.map ~f:(fun j -> i, j)) |> List.concat
;;

let distance p1 p2 =
  let x1, y1, z1 = p1 in
  let x2, y2, z2 = p2 in
  let dx = x1 - x2 in
  let dy = y1 - y2 in
  let dz = z1 - z2 in
  Float.sqrt (Float.of_int ((dx * dx) + (dy * dy) + (dz * dz)))
;;

let parse_input (input : string list) : point list =
  input
  |> List.map ~f:(fun line ->
    match line |> String.split ~on:',' |> List.map ~f:Int.of_string with
    | [ x; y; z ] -> x, y, z
    | _ -> failwith "")
;;

let rec parent disjoint_set x =
  match Array.get disjoint_set x with
  | px when x = px -> px
  | px -> parent disjoint_set px
;;

let join disjoint_set x y =
  let _ = Array.set disjoint_set x y in
  disjoint_set
;;

let set_parents disjoint_set : int array =
  let len = Array.length disjoint_set in
  Array.init len ~f:(fun i -> parent disjoint_set i)
;;

let all_values_equal arr =
  arr |> Array.to_list |> List.all_equal ~equal:( = ) |> Option.is_some
;;

let solve_part1 input n =
  let points = input |> parse_input in
  let num_points = List.length points in
  let disjoint_set = Array.init num_points ~f:(fun i -> i) in
  let point_to_idx =
    points
    |> List.mapi ~f:(fun i p -> i, p)
    |> List.fold_left
         ~init:(Hashtbl.create (module Point))
         ~f:(fun acc (i, p) ->
           let _ = Hashtbl.add acc ~key:p ~data:i in
           acc)
  in
  match
    cartesian_product points points
    |> List.filter ~f:(fun ((x1, y1, z1), (x2, y2, z2)) ->
      (*
         This is an arbitrary value that is guaranteed to have opposite sides for the same pair in both positions.
         It might also be 0 if the points are equal. We want to filter out these pairs too, so we only take those with negative 'dir'.
      *)
      let dir = x1 - x2 + y1 - y2 + z1 - z2 in
      dir < 0)
    |> List.map ~f:(fun (p1, p2) -> p1, p2, distance p1 p2)
    |> List.sort ~compare:(fun (_, _, d1) (_, _, d2) -> Float.compare d1 d2)
    |> (fun l -> List.take l n)
    |> List.fold ~init:disjoint_set ~f:(fun disjoint_set (p1, p2, _) ->
      let p1 = Hashtbl.find point_to_idx p1 |> Option.value_exn ~message:"" in
      let p2 = Hashtbl.find point_to_idx p2 |> Option.value_exn ~message:"" in
      let parent1 = parent disjoint_set p1 in
      let parent2 = parent disjoint_set p2 in
      if parent1 = parent2 then disjoint_set else join disjoint_set parent1 parent2)
    |> set_parents
    |> Array.to_list
    |> List.fold_left
         ~init:(Hashtbl.create (module Int))
         ~f:(fun acc group ->
           let _ =
             Hashtbl.update acc group ~f:(fun value ->
               match value with
               | Some count -> count + 1
               | None -> 1)
           in
           acc)
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (_, v) -> v)
    |> List.sort ~compare:(fun a b -> b - a)
    |> fun l -> List.take l 3
  with
  | [ a; b; c ] -> a * b * c
  | _ -> failwith ""
;;

let solve_part2 input n =
  let points = input |> parse_input in
  let num_points = List.length points in
  let disjoint_set = Array.init num_points ~f:(fun i -> i) in
  let point_to_idx =
    points
    |> List.mapi ~f:(fun i p -> i, p)
    |> List.fold_left
         ~init:(Hashtbl.create (module Point))
         ~f:(fun acc (i, p) ->
           let _ = Hashtbl.add acc ~key:p ~data:i in
           acc)
  in
  let (x1, _, _), (x2, _, _) =
    cartesian_product points points
    |> List.filter ~f:(fun ((x1, y1, z1), (x2, y2, z2)) ->
      (*
         This is an arbitrary value that is guaranteed to have opposite sides for the same pair in both positions.
         It might also be 0 if the points are equal. We want to filter out these pairs too, so we only take those with negative 'dir'.
      *)
      let dir = x1 - x2 + y1 - y2 + z1 - z2 in
      dir < 0)
    |> List.map ~f:(fun (p1, p2) -> p1, p2, distance p1 p2)
    |> List.sort ~compare:(fun (_, _, d1) (_, _, d2) -> Float.compare d1 d2)
    |> (fun l -> List.take l (n * 100))
    |> List.fold_until
         ~init:disjoint_set
         ~f:(fun disjoint_set (point1, point2, _) ->
           let p1 = Hashtbl.find point_to_idx point1 |> Option.value_exn ~message:"" in
           let p2 = Hashtbl.find point_to_idx point2 |> Option.value_exn ~message:"" in
           let parent1 = parent disjoint_set p1 in
           let parent2 = parent disjoint_set p2 in
           let disjoint_set =
             if parent1 = parent2 then disjoint_set else join disjoint_set parent1 parent2
           in
           let disjoint_set = set_parents disjoint_set in
           if all_values_equal disjoint_set
           then Stop (point1, point2)
           else Continue disjoint_set)
         ~finish:(fun _ -> (3, 0, 0), (7, 0, 0))
  in
  x1 * x2
;;

let sample_input = "sample.txt" |> In_channel.read_lines
let input = "input.txt" |> In_channel.read_lines
let _ = print_endline "================"
let _ = print_endline "==   Part 1   =="
let _ = print_endline "================"
let _ = printf "%d\n" (solve_part1 sample_input 10)
let _ = printf "%d\n" (solve_part1 input 1000)
let _ = print_endline "================"
let _ = print_endline "==   Part 2   =="
let _ = print_endline "================"
let _ = printf "%d\n" (solve_part2 sample_input 10)
let _ = printf "%d\n" (solve_part2 input 1000)
