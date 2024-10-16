val num_terrs_owned : int -> int
(*[num_terrs_owned player] is the number of countries that [player] controls*)

val troops_on_start : int -> int
(*[troops_on_start player] is the number of troops [player] gets at the start of
  their turn as per Risk rules*)

val place_troops : int -> int -> unit
(*[place_troops player num_troops] allows [player] to place [num_troops] troops
  in the countries [player] owns. [player] must place all troops*)

val attack : int -> unit
(*[attack player] allows [player] to attack from an owned country to an adjacent
  enemy country*)

val fortify : int -> unit
(*[fortify player] allows [player] to fortify troops between two connected
  countries*)

val countries : string list
(*[countries] is a list of the names of every country in Risk*)

val owned_by : int -> string option array
(*[owned_by player] is a string option array where [Some c] represents a country
  [c] that [player] owns *)

val troops_from_cont : int -> int
(*[troops_from_cont player] is the number of bonus troops [player] recieves from
  owning full continents as per Risk rules*)

val valid_num_troops : int -> string -> int
(*[valid_num_troops num_troops name] is checks user input and returns a valid
  number of troops that can be placed in the country with name [name]*)

val is_reachable : Map.country -> Map.country -> bool
(*[is_reachable c1 c2] is [true] when there is a path connecting [c1] to [c2]
  which is owned by one player, and is [false] otherwise**)

val get_int : unit -> int
(* [get_int ()] prompts the user for the number of dice to roll within the
   allowed maximum and returns that number. *)

val get_num_dice : string -> int -> int
(* [get_num_dice ]Rolls a specified number of dice and returns the results in
   descending order. *)

val roll_dice : int -> int list
(* [roll_dice dice] rolls [dice] and is the results in descending order *)

val compare_rolls : int list -> int list -> int ref -> int ref -> unit
(* [compare_rolls attacker defender attack_troops defense_troops] the results of
   dice rolls between [attacker] and [defender] and adjusts their troop numbers
   accordingly. *)

val move_troops : Map.country -> Map.country -> unit
(*[move_troops c1 c2] moves an inputted number of troops from c1 to c2*)

val get_valid_country : string -> int -> bool -> Map.country
(*[get_valid_country prompt player is_attacker] is a country that [player] can
  attack from or a country that [player will attack]*)

val player_has_territories : int -> bool
(*[player_has_territories player] is [true] if the player has at least one
  territory, and [false] otherwise*)
