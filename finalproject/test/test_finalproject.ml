(* test_suite.ml *)
open OUnit2
open Finalproject.Player
open Finalproject.Turn
open Finalproject.Map

(* Load the test map *)
let test_countries2 =
  [
    ("Alaska", "North America");
    ("Alberta", "North America");
    ("Ontario", "North America");
    ("Western America", "North America");
    ("Eastern America", "North America");
    ("Quebec", "North America");
  ]

let test_countries =
  [
    "Alaska";
    "Alberta";
    "Ontario";
    "Western America";
    "Eastern America";
    "Quebec";
  ]

let setup_game () =
  let venezuela =
    {
      name = "Venezuela";
      player = Some 1;
      continent = "South America";
      troops = Some 5;
      neighbours = [];
    }
  in
  let peru =
    {
      name = "Peru";
      player = Some 1;
      continent = "South America";
      troops = Some 3;
      neighbours = [ venezuela ];
    }
  in
  let brazil =
    {
      name = "Quebec";
      player = Some 2;
      continent = "North America";
      troops = Some 2;
      neighbours = [ peru ];
    }
  in
  let alaska =
    {
      name = "Alaska";
      player = Some 1;
      continent = "North America";
      troops = Some 5;
      neighbours = [];
    }
  in
  let alberta =
    {
      name = "Alberta";
      player = Some 1;
      continent = "North America";
      troops = Some 3;
      neighbours = [ alaska ];
    }
  in
  let quebec =
    {
      name = "Quebec";
      player = Some 2;
      continent = "North America";
      troops = Some 2;
      neighbours = [ alaska ];
    }
  in
  let kamchatka =
    {
      name = "Kamchatka";
      player = Some 2;
      continent = "Asia";
      troops = Some 2;
      neighbours = [ alaska ];
    }
  in
  alaska.neighbours <- [ alberta; quebec ];
  alaska.neighbours <- [ kamchatka ];
  alberta.neighbours <- [ alaska ];
  quebec.neighbours <- [ alaska ];
  brazil.neighbours <- [ peru ];
  brazil.neighbours <- [ venezuela ];

  [ alaska; alberta; quebec ]

let worldMap = ref []

let initialize_world_map () =
  let mock_countries = setup_game () in
  worldMap := mock_countries

let test_num_terrs_owned _ =
  initialize_world_map ();
  let num_territories = num_terrs_owned 1 in
  assert_equal 2 num_territories ~printer:string_of_int

let test_is_reachable_one _ =
  initialize_world_map ();
  let alaska = List.find (fun c -> c.name = "Alaska") !worldMap in
  let alberta = List.find (fun c -> c.name = "Alberta") !worldMap in
  assert_bool "Alaska\n should be\n  reachable from Alberta"
    (is_reachable alberta alaska)

let test_is_reachable_two _ =
  initialize_world_map ();
  let alaska = List.find (fun c -> c.name = "Alaska") !worldMap in
  let quebec = List.find (fun c -> c.name = "Quebec") !worldMap in
  assert_bool
    "Quebec should not\n be\n  reachable from Alaska for different players"
    (not (is_reachable alaska quebec))

let test_placing _ =
  initialize_world_map ();
  let initial_troops =
    match List.find (fun c -> c.name = "Alaska") !worldMap with
    | { troops = Some t; _ } -> t
    | _ -> failwith "Setup failed"
  in
  place_troops 1 3;
  let after_troops =
    match List.find (fun c -> c.name = "Alaska") !worldMap with
    | { troops = Some t; _ } -> t
    | _ -> failwith "Troop placement failed"
  in
  assert_equal (initial_troops + 3) after_troops ~printer:string_of_int

let suite1 =
  "turn_tests"
  >::: [
         "test_num_terrs_owned" >:: test_num_terrs_owned;
         "test_is_reachable_positive" >:: test_is_reachable_one;
         "test_is_reachable_negative" >:: test_is_reachable_two;
         "test_place_troops" >:: test_placing;
       ]

let test_map = List.map construct_map_element test_countries2

let add_test_link (cont1, cont2) =
  try
    let country1 = List.assoc cont1 test_map in
    let country2 = List.assoc cont2 test_map in
    country1.neighbours <- country2 :: country1.neighbours;
    country2.neighbours <- country1 :: country2.neighbours
  with _ -> print_endline (cont1 ^ " " ^ cont2)

let test_links =
  [
    ("Alaska", "Alberta");
    ("Alberta", "Ontario");
    ("Ontario", "Quebec");
    ("Western America", "Eastern America");
  ]

let () = List.iter add_test_link test_links

(* Override the worldMap with the test_map for testing *)
let worldMap = test_map

(* Test cases *)

let test_num_terrs_owned _ =
  List.iter (fun (_, country) -> country.player <- Some 0) test_map;
  assert_equal 6 (num_terrs_owned 0);
  List.iter (fun (_, country) -> country.player <- None) test_map;
  assert_equal 0 (num_terrs_owned 0)

let test_troops_from_cont _ =
  List.iter (fun (_, country) -> country.player <- Some 0) test_map;
  assert_equal 24 (troops_from_cont 0);
  List.iter (fun (_, country) -> country.player <- None) test_map;
  assert_equal 0 (troops_from_cont 0)

let test_troops_on_start _ =
  assert_equal 24 (troops_on_start 0);
  assert_equal 24 (troops_on_start 1)

let test_player_has_territories _ =
  List.iter (fun (_, country) -> country.player <- Some 0) test_map;
  assert_equal (player_has_territories 0) true;
  List.iter (fun (_, country) -> country.player <- None) test_map;
  assert_equal (player_has_territories 0) false

let test_place_troops _ =
  List.iter (fun (_, country) -> country.player <- Some 0) test_map;
  place_troops 0 10;
  let alaska = List.assoc "Alaska" test_map in
  assert_equal (Some 11) alaska.troops;
  (* assuming initial troops are 1 *)
  let alberta = List.assoc "Alberta" test_map in
  assert_equal (Some 2) alberta.troops (* assuming initial troops are 1 *)

let test_isNeighbour _ =
  let alaska = List.assoc "Alaska" test_map in
  let alberta = List.assoc "Alberta" test_map in
  assert (isNeighbour alaska alberta);
  let quebec = List.assoc "Quebec" test_map in
  assert (not (isNeighbour alaska quebec))

let test_valid_num_troops _ =
  let troops = valid_num_troops 10 "Alaska" in
  assert_equal 5 troops;
  let troops = valid_num_troops 10 "Alberta" in
  assert_equal 3 troops

let test_get_valid_country _ =
  let country = get_valid_country "Enter a country:" 0 true in
  assert_equal "Alaska" country.name;
  let country = get_valid_country "Enter a country:" 1 true in
  assert_equal "Alberta" country.name

let test_roll_dice _ =
  let rolls = roll_dice 3 in
  assert_equal 3 (List.length rolls);
  List.iter (fun x -> assert (x >= 1 && x <= 6)) rolls;
  let rolls = roll_dice 2 in
  assert_equal 2 (List.length rolls)

let test_compare_rolls _ =
  let attack_troops = ref 3 in
  let defend_troops = ref 2 in
  compare_rolls [ 6; 5; 4 ] [ 3; 2 ] attack_troops defend_troops;
  assert_equal 3 !attack_troops;
  assert_equal 0 !defend_troops;
  let attack_troops = ref 3 in
  let defend_troops = ref 2 in
  compare_rolls [ 2; 1 ] [ 3; 2 ] attack_troops defend_troops;
  assert_equal 1 !attack_troops;
  assert_equal 2 !defend_troops

let test_move_troops _ =
  let alaska = List.assoc "Alaska" test_map in
  let alberta = List.assoc "Alberta" test_map in
  alaska.troops <- Some 5;
  move_troops alaska alberta;
  assert_equal (Some 1) alaska.troops;
  assert_equal (Some 4) alberta.troops

let test_attack _ =
  let alaska = List.assoc "Alaska" test_map in
  let alberta = List.assoc "Alberta" test_map in
  alaska.player <- Some 0;
  alberta.player <- Some 1;
  alaska.troops <- Some 5;
  alberta.troops <- Some 2;
  attack 0;
  assert_equal (Some 0) alberta.player;
  assert_equal (Some 1) alberta.troops

let test_fortify _ =
  let alaska = List.assoc "Alaska" test_map in
  let alberta = List.assoc "Alberta" test_map in
  alaska.player <- Some 0;
  alberta.player <- Some 0;
  alaska.troops <- Some 5;
  fortify 0;
  assert_equal (Some 1) alaska.troops;
  assert_equal (Some 4) alberta.troops

let test_auto_assign _ =
  auto_assign test_countries 0;
  List.iter (fun (_, country) -> assert (country.player <> None)) test_map

let test_claim_countries _ =
  claim_countries test_countries 0;
  List.iter (fun (_, country) -> assert (country.player <> None)) test_map

let test_world_map _ =
  assert_equal 6 (List.length worldMap);
  List.iter (fun (_, country) -> assert (country.player <> None)) worldMap

let test_sub_troops _ =
  let initial_troops = troops_state.(0) in
  subTroops 0 5;
  assert_equal (initial_troops - 5) troops_state.(0)

let test_add_troops _ =
  let initial_troops = troops_state.(0) in
  addTroops 0 5;
  assert_equal (initial_troops + 5) troops_state.(0)

let test_claim_country_validity _ =
  claim_countries [ "Alaska"; "Alberta"; "Ontario" ] 0;
  let alaska = List.assoc "Alaska" test_map in
  let alberta = List.assoc "Alberta" test_map in
  let ontario = List.assoc "Ontario" test_map in
  assert_equal alaska.player (Some 0);
  assert_equal alberta.player (Some 0);
  assert_equal ontario.player (Some 0)

let test_auto_assign_validity _ =
  auto_assign [ "Western America"; "Eastern America"; "Quebec" ] 1;
  let western_america = List.assoc "Western America" test_map in
  let eastern_america = List.assoc "Eastern America" test_map in
  let quebec = List.assoc "Quebec" test_map in
  assert_equal western_america.player (Some 1);
  assert_equal eastern_america.player (Some 1);
  assert_equal quebec.player (Some 1)

(* Combine all tests into a test suite *)
let suite =
  "Test Suite"
  >::: [
         "test_num_terrs_owned" >:: test_num_terrs_owned;
         "test_troops_from_cont" >:: test_troops_from_cont;
         "test_troops_on_start" >:: test_troops_on_start;
         "test_player_has_territories" >:: test_player_has_territories;
         "test_place_troops" >:: test_place_troops;
         "test_isNeighbour" >:: test_isNeighbour;
         "test_valid_num_troops" >:: test_valid_num_troops;
         "test_get_valid_country" >:: test_get_valid_country;
         "test_roll_dice" >:: test_roll_dice;
         "test_compare_rolls" >:: test_compare_rolls;
         "test_move_troops" >:: test_move_troops;
         "test_attack" >:: test_attack;
         "test_fortify" >:: test_fortify;
         "test_auto_assign" >:: test_auto_assign;
         "test_claim_countries" >:: test_claim_countries;
         "test_world_map" >:: test_world_map;
         "test_sub_troops" >:: test_sub_troops;
         "test_add_troops" >:: test_add_troops;
         "test_claim_country_validity" >:: test_claim_country_validity;
         "test_auto_assign_validity" >:: test_auto_assign_validity;
       ]

let () = run_test_tt_main suite
