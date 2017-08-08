type position = {
  x : int;
  y : int;
}

let string_of_position pos =
  (string_of_int pos.x) ^ "," ^ (string_of_int pos.y)

type power_up =
  | Power
  | Productivity
  | Empty

type case =
  | Indestructible
  | Rektable of power_up
  | Free of power_up

type grid = case array array

type bomb = {
  b_position : position;
  auteur : int;
  b_power : int;
  time_left : int;
}

type player = {
  name : string;
  productivity : int;
  p_power : int;
  score : int;
  deployable : int;
  p_position : position;
  is_alive : bool;
}

type game_state = {
  nb_players : int;
  turns_left : int;
  players : player list;
  bombs : bomb list;
  size : position;
  grid : grid;
}

let string_of_case c =
  match c with
  | Indestructible -> "X"
  | Rektable(_) -> "O"
  | Free(_) -> "G"

let print_game_state gs =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      print_string (string_of_case cell)
    ) row;
    print_string "\n"
  ) gs.grid
