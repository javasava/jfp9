open Game
open Movement
open Command

let () =
  let my_name = "JAVASAVA" in
  init_game my_name (fun gs ->
    let me = List.find (fun p -> p.name = my_name) gs.players in
    let candidates = valid_neighbours gs me.p_position in
    let candidates = List.filter (fun {x; y} -> match gs.grid.(y).(x) with
      | Free(_) -> true
      | _ -> false
    ) candidates in
    match candidates with
    | dst::_ -> move_to me.p_position dst
    | [] -> nop ()
  )
