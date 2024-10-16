let countries =
  let r = Map.read_lines "lib/countries.txt" in
  match List.split r with
  | cs, _ -> cs

let map = Map.map ()
let num_players = ref 0

let test_countries =
  [
    "Alaska";
    "Alberta";
    "Ontario";
    "Western America";
    "Eastern America";
    "Quebec";
  ]

let () = Random.self_init ()
let soldiers = [| 40; 35; 30; 25; 20 |]

(* Prints out a summary of the rules and stages of Risk. *)
let print_rules () =
  print_endline
    "Welcome to Risk! Here's a summary of the game rules and stages:\n";

  print_endline "OBJECTIVE:";
  print_endline
    "  The objective of Risk is to conquer every territory on the board by \
     attacking other players' territories, ";
  print_endline
    "  and thus eliminating them from the game. The last player remaining with \
     any territories wins the game.\n";

  print_endline "STAGES OF GAMEPLAY:";
  print_endline "  1. Setup Phase:";
  print_endline "     - Players decide order of play.";
  print_endline
    "     - Players claim territories until all territories are claimed.";
  print_endline "     - Players place initial armies on their territories.";

  print_endline "  2. Playing Phase:";
  print_endline "     - Each turn consists of three parts:";
  print_endline
    "       a. Getting and placing new armies based on territory count, and \
     control of continents.";
  print_endline
    "       b. Attacking other players' territories to capture them.";
  print_endline
    "       c. Fortifying your position by moving armies between your adjacent \
     territories.";

  print_endline "  3. Ending Phase:";
  print_endline
    "     - Players may end their turn and pass play to the next player.";
  print_endline
    "     - The game ends when one player controls all the territories or when \
     the players decide to end the game.\n";

  print_endline "ADDITIONAL RULES:";
  print_endline
    "  - Players earn armies at the start of their turn by controlling \
     territories and continents.";
  print_endline
    "  - You can attack as many times as you want and move as many armies as \
     possible where you have more than one army.";
  print_endline
    "  - Fortifying (moving armies to adjacent territories) is done at the end \
     of your turn and is optional but can only be done once per turn.\n";

  print_endline
    "For a detailed list of rules, consider consulting the official Risk \
     rulebook or online resources."

let rec number_of_players first =
  if first = true then print_rules ();
  try
    let () = print_endline "How many players do you want: " in
    let input = read_line () in
    let num = int_of_string input in
    num_players := num;
    if num < 2 || num > 6 then
      raise (Invalid_argument "Incorrect Number of Players")
    else num
  with _ ->
    print_endline "Please enter a valid number of players (between 2 and 6)";
    number_of_players false

let num_players = number_of_players true
let troops_state = Array.make num_players soldiers.(num_players - 2)

let bonusList : (string * int ref) list =
  [
    ("North America", ref 0);
    ("South America", ref 0);
    ("Africa", ref 0);
    ("Asia", ref 0);
    ("Europe", ref 0);
    ("Australia", ref 0);
  ]

(* let continentOfOne mass = let massList = List.filter (fun (_, ctry) ->
   ctry.continent = mass) map in match massList with | (_, cry) -> let comp =
   cry.player | *)

let remove_country country1 country2 =
  if country1 = country2 then false else true

let rec auto_assign rem_countries player_turn =
  match rem_countries with
  | [] -> ()
  | _ ->
      let () = Random.self_init () in
      let input =
        List.nth rem_countries (Random.int (List.length rem_countries))
      in
      let country = List.assoc input map in
      let () = country.troops <- Some 1 in
      let () = country.player <- Some player_turn in
      auto_assign
        (List.filter (remove_country input) rem_countries)
        ((player_turn + 1) mod num_players)

let rec claim_countries rem_countries player_turn =
  match rem_countries with
  | [] -> ()
  | _ ->
      let () =
        print_endline
          ("Player " ^ string_of_int (player_turn + 1) ^ " choose a country")
      in
      let input = read_line () in
      if List.mem input rem_countries then
        let country = List.assoc input map in
        let () = country.troops <- Some 1 in
        let () = country.player <- Some player_turn in
        claim_countries
          (List.filter (remove_country input) rem_countries)
          ((player_turn + 1) mod num_players)
      else
        let () = print_endline "Please enter a valid country " in
        claim_countries rem_countries player_turn

let print_owners () =
  for i = 0 to List.length map - 1 do
    match List.nth map i with
    | ( str,
        {
          Map.name = _;
          player = Some x;
          troops = _;
          neighbours = _;
          continent = _;
        } ) ->
        let () = print_string str in
        let () = print_endline (" , Player " ^ string_of_int (x + 1)) in
        ()
    | _ -> ()
  done

let worldMap =
  let () =
    print_endline
      "Would you like to auto assign countries. If yes, enter yes. If no, \
       press enter: "
  in
  let input = read_line () in
  if input = "yes" then
    let () = auto_assign countries 0 in
    map
  else
    let () = claim_countries countries 0 in
    map

let subTroops player_index n =
  troops_state.(player_index) <- troops_state.(player_index) - n

let addTroops player_index n =
  troops_state.(player_index) <- troops_state.(player_index) + n
