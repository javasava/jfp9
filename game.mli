type position = {
  x : int;
  y : int;
}

val string_of_position : position -> string

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

val string_of_case : case -> string
val print_game_state : game_state -> unit
