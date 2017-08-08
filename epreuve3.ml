open Queue

open Game
open Movement
open Command

let complement gs dangerous =
  let (_, l) = Array.fold_left (fun (y, acc) row ->
    let (_, l) = Array.fold_left (fun (x, acc) _ ->
      let pos = {x; y} in
      let acc = if not (List.mem pos dangerous) then pos::acc else acc in
      (x + 1, acc)
    ) (0, acc) row
    in (y + 1, l@acc)
  ) (0, []) gs.grid
  in
  l

let closest gs from l =
  let destinations = List.fold_left (fun acc dest ->
    let (next, d) = shortest_path gs from dest is_free in
    match next with
    | Some(next) -> (dest, next, d)::acc
    | None -> acc
  ) [] l
  in
  List.fold_left (fun (mdest, mnext, md) (dest, next, d) ->
    if d < md then (dest, next, d) else (mdest, mnext, md)
  ) (from, from, max_int) destinations

let rec bomb_radius gs b =
  let rec step (l, power) delta pos =
    if power = 0 then (l, power) else
    let next = pos +: delta in
    if not (is_valid gs next) then (l, power) else
    match gs.grid.(pos.y).(pos.x) with
    | Indestructible -> (l, power)
    | _ ->
      let l = next::l in
      let l = try
        let nextb = List.find (fun b -> b.b_position = next) gs.bombs in
        l @ (bomb_radius gs nextb)
      with Not_found -> l
      in
      (l, power - 1)
  in
  (fst (step ([], b.b_power) {x = 1; y = 0} b.b_position)) @
  (fst (step ([], b.b_power) {x = 0; y = 1} b.b_position)) @
  (fst (step ([], b.b_power) {x = -1; y = 0} b.b_position)) @
  (fst (step ([], b.b_power) {x = 0; y = -1} b.b_position))

let () =
  let my_name = "JAVASAVA" in
  init_game my_name (fun gs ->
    let me = List.find (fun p -> p.name = my_name) gs.players in

    let dangerous = List.fold_left (fun acc b ->
      acc @ (bomb_radius gs b)
    ) [] gs.bombs in
    if List.exists (fun pos -> pos = me.p_position) dangerous then
      let safe = complement gs dangerous in
      let (_, next, _) = closest gs me.p_position safe in
      move_to me.p_position next
    else
      let candidates = find_all_bonus gs.grid in
      match candidates with
      | target::_ ->
        let (next, _) = shortest_path gs me.p_position target (fun c -> match c with
          | Free(_) | Rektable(_) -> true
          | _ -> false
        ) in
        (match next with
        | Some(next) ->
          (match gs.grid.(next.y).(next.x) with
          | Rektable(_) -> deploy ()
          | Free(_) -> move_to me.p_position next
          | _ -> raise (Failure "savapatrojava"))
        | None -> nop ())
      | _ -> nop ()
  )
