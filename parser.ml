open Game
(*
let read_state =

  let nbj_turnLeft = read_line () in

  if Str.string_match nbj_turnLeft 0 then begin

  end else assert false
*)


let read_state () =
  let nbj, turnsLeft = Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y) in

  let rec read_players playersLeft acc =
    if playersLeft = 0 then acc
    else begin
      let newPlayer = Scanf.sscanf (read_line ()) "%s %d %d %d %d %d %d %d"
        (fun name a b c d e f g -> {
          name = name;
          productivity = a;
          p_power = b;
          score = c;
          deployable = d;
          p_position = {x = f; y = e};
          is_alive = (g = 1);
        })
      in
      read_players (playersLeft - 1) [newPlayer]@acc
    end
  in

  let players = List.rev (read_players nbj []) in

  let sizeY, sizeX = Scanf.sscanf (read_line ()) "%d %d" (fun x y -> x, y) in

  let grid = Array.make_matrix sizeY sizeX Indestructible in

  let rec read_bombs_and_grid accBombs currentLine =
    if currentLine >= sizeY then accBombs
    else begin
      let l = read_line () in
      let l = Str.split (Str.regexp "|") l in

      let newBombs, _ = List.fold_left (fun (oldBombs, oldX) block ->
        let splittedBlock = Str.split (Str.regexp "/") block in
        let blockType, codes =
          match splittedBlock with
          | x::s -> x, s
          | _ -> print_string "Invalid block"; assert false
        in

        let bombs = List.fold_left (fun acc l ->
          let x, y, z = match Str.split (Str.regexp ",") l with
          | x::y::z::[] -> x, y, z
          | _ -> print_string "Invalid bomb"; assert false in

          [{
            b_position = {x = oldX; y = currentLine};
            auteur = int_of_string x;
            b_power = int_of_string x;
            time_left = int_of_string x;
          }]@acc
        ) [] codes in

        if blockType <> "" then begin
          grid.(currentLine).(oldX) <-
            if blockType = "X" then Indestructible
            else if blockType = "O" then Rektable(Empty)
            else if blockType = "Y" then Rektable(Power)
            else if blockType = "Z" then Rektable(Productivity)
            else if blockType = "G" then Free(Empty)
            else if blockType = "H" then Free(Power)
            else if blockType = "I" then Free(Productivity)
            else begin print_string "invalid block type"; Printf.printf "got: %s" blockType; assert false end;
          (bombs@oldBombs, oldX + 1)
        end else (bombs@oldBombs, oldX)
      ) ([], 0) l
      in
      read_bombs_and_grid (newBombs@accBombs) (currentLine + 1)

    end

  in

  let bombs = read_bombs_and_grid [] 0 in

  if (read_line ()) <> "#" then begin print_string "naab"; assert false end else

  {
    nb_players = nbj;
    turns_left = turnsLeft;
    players = players;
    bombs = bombs;
    size = {x = sizeX; y = sizeY};
    grid = grid;
  }
