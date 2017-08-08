open Game
open Parser

let send_command cmd =
  Printf.printf "%s\n%!" cmd

let init_game name step =
  if read_line () <> "NAME?" then raise (Failure "Nom non nom");
  send_command name;
  if read_line () <> "#" then raise (Failure "C'pas un séparateur ça...");

  let rec f () =
    try step (read_state ()); f ()
    with End_of_file -> ()
  in
  f ()

let nop () =
  send_command "NOP"

let move_to src dst =
  if src.x > dst.x then send_command "LEFT"
  else if src.y > dst.y then send_command "UP"
  else if src.x < dst.x then send_command "RIGHT"
  else if src.y < dst.y then send_command "DOWN"
  else nop ()

let deploy () =
  send_command "DEPLOY"
