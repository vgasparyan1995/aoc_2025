open Core
open Stdio

let parse_input input =
  input
  |> List.map ~f:(fun line ->
    match line |> String.split_on_chars ~on:[ ',' ] |> List.map ~f:Int.of_string with
    | [ x; y ] -> x, y
    | _ -> failwith "")
;;

let range a b = List.init (b - a) ~f:(fun i -> a + i)

let pairs list =
  let arr = Array.of_list list in
  let len = Array.length arr in
  range 0 len
  |> List.map ~f:(fun i -> range (i + 1) len |> List.map ~f:(fun j -> arr.(i), arr.(j)))
  |> List.concat
;;

let area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

let into_polygon points =
  match points with
  | [] -> []
  | hd :: rest -> hd :: List.rev (hd :: rest)
;;

type edge =
  | Horizontal of (int * int) * int
  | Vertical of int * (int * int)

let edges ((x1, y1), (x2, y2)) =
  [ Vertical (x1, (y1, y2))
  ; Vertical (x2, (y1, y2))
  ; Horizontal ((x1, x2), y1)
  ; Horizontal ((x1, x2), y2)
  ]
;;

let make_edge (x1, y1) (x2, y2) =
  if x1 = x2
  then Vertical (x1, (y1, y2))
  else if y1 = y2
  then Horizontal ((x1, x2), y1)
  else failwith ""
;;

let crossing e1 e2 =
  match e1, e2 with
  | Horizontal _, Horizontal _ | Vertical _, Vertical _ -> false
  | Horizontal ((x1, x2), y), Vertical (x, (y1, y2))
  | Vertical (x, (y1, y2)), Horizontal ((x1, x2), y) ->
    let x_lo = min x1 x2 in
    let x_hi = max x1 x2 in
    let y_lo = min y1 y2 in
    let y_hi = max y1 y2 in
    x > x_lo && x < x_hi && y > y_lo && y < y_hi
;;

let inside_polygon polygon edge =
  let len = Array.length polygon in
  range 1 len
  |> List.map ~f:(fun i -> make_edge polygon.(i - 1) polygon.(i))
  |> List.for_all ~f:(fun polygon_edge -> not (crossing polygon_edge edge))
;;

let point_in_polygon polygon (px_f, py_f) =
  let n = Array.length polygon in
  let count = ref 0 in
  for i = 0 to n - 2 do
    let x1, y1 = polygon.(i) in
    let x2, y2 = polygon.(i + 1) in
    let y1_f = Float.of_int y1 in
    let y2_f = Float.of_int y2 in
    let x1_f = Float.of_int x1 in
    let x2_f = Float.of_int x2 in
    let is_between_y =
      not (Bool.equal (Float.compare y1_f py_f > 0) (Float.compare y2_f py_f > 0))
    in
    if is_between_y
    then (
      let intersect_x = x1_f +. ((py_f -. y1_f) *. (x2_f -. x1_f) /. (y2_f -. y1_f)) in
      if Float.compare intersect_x px_f > 0 then incr count)
  done;
  !count mod 2 = 1
;;

let vertex_inside_rect (rx1, ry1) (rx2, ry2) (vx, vy) =
  let x_min = min rx1 rx2 in
  let x_max = max rx1 rx2 in
  let y_min = min ry1 ry2 in
  let y_max = max ry1 ry2 in
  vx > x_min && vx < x_max && vy > y_min && vy < y_max
;;

let solve_part1 input =
  input |> parse_input |> pairs |> List.map ~f:area |> List.fold_left ~init:0 ~f:max
;;

let solve_part2 input =
  let points = input |> parse_input in
  let polygon = points |> into_polygon |> Array.of_list in
  points
  |> pairs
  |> List.filter ~f:(fun pairs ->
    let (x1, y1), (x2, y2) = pairs in
    let cx = Float.of_int (x1 + x2) /. 2.0 in
    let cy = Float.of_int (y1 + y2) /. 2.0 in
    let no_crossing = pairs |> edges |> List.for_all ~f:(inside_polygon polygon) in
    let center_in_polygon = point_in_polygon polygon (cx, cy) in
    let no_vertex_inside =
      Array.for_all polygon ~f:(fun v -> not (vertex_inside_rect (x1, y1) (x2, y2) v))
    in
    no_crossing && center_in_polygon && no_vertex_inside)
  |> List.map ~f:area
  |> List.fold_left ~init:0 ~f:max
;;

let sample_input = "sample.txt" |> In_channel.read_lines
let input = "input.txt" |> In_channel.read_lines
let _ = printf "================"
let _ = printf "==   Part 1   =="
let _ = printf "================"
let _ = printf "%d\n" (solve_part1 sample_input)
let _ = printf "%d\n" (solve_part1 input)
let _ = printf "================"
let _ = printf "==   Part 2   =="
let _ = printf "================"
let _ = printf "%d\n" (solve_part2 sample_input)
let _ = printf "%d\n" (solve_part2 input)
