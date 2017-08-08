open Game

let (+:) a b =
  {x = a.x + b.x; y = a.y + b.y}

(* position -> position list *)
let neighbours n =
  [
    {x = n.x - 1; y = n.y};
    {x = n.x    ; y = n.y - 1};
    {x = n.x + 1; y = n.y};
    {x = n.x    ; y = n.y + 1}
  ]

let is_valid gs p =
  p.x < gs.size.x && p.x >= 0 &&
  p.y < gs.size.y && p.y >= 0

(* position list -> position list *)
let valid_neighbours gs pos =
  List.filter (is_valid gs) (neighbours pos)

(* game_state -> position -> position -> position option *)
let shortest_path gs from dest p =
  let m = Array.make_matrix gs.size.x gs.size.y (max_int, None) in
  m.(from.y).(from.x) <- (0, None);
  let rec f pos =
    List.iter (fun n ->
      if p gs.grid.(n.y).(n.x) then
        let d =  1 + fst (m.(pos.y).(pos.x)) in
        if d < fst (m.(n.x).(n.y)) then (
          m.(n.y).(n.x) <- (d, Some(pos));
          f n
        )
    ) (valid_neighbours gs pos)
  in
  f from;
  let rec backtrack pos =
    match snd m.(pos.y).(pos.x) with
    | Some(prev) -> if prev = from then Some(pos) else backtrack prev
    | None -> None
  in
  (backtrack dest, fst m.(dest.y).(dest.x))

(* case -> bool*)
let is_productivity = function
  | Free(Productivity) | Rektable(Productivity) -> true
  | _ -> false

let is_power = function
  | Free(Power) | Rektable(Power) -> true
  | _ -> false

let is_empty = function
  | Free(Empty) | Rektable(Empty) -> true
  | _ -> false

let is_free = function
  | Free(_) -> true
  | _ -> false

let is_bonus c = (is_power c) || (is_productivity c)

(* (case -> bool) -> grid -> position list *)

let grid_filter f g =
  let res = ref [] in
  for i = 0 to Array.length g - 1 do
    for j = 0 to Array.length g.(i) - 1 do
      if f (g.(i).(j)) then res := {x = j; y = i}::(!res)
    done
  done;
  !res

  (* Array.fold_left (fun (p, acc) a ->
    Array.fold_left (fun (j, acc) e ->
      if f e then (j + 1, e::(snd acc)) else (j + 1, snd acc)
    ) acc a
  ) (-1,-1,[]) g *)

(* grid -> position list *)
let find_power_up = grid_filter is_power
let find_productivity_up = grid_filter is_productivity
let find_all_bonus g = grid_filter is_bonus g
let find_empty = grid_filter is_empty
