open Map
open Player

let countries = Player.countries

(*[owned_by player] is a string option array, where each [Some s] is the name of
  a country that [player] owns*)
let owned_by p =
  let terrs = Array.make 42 None in
  let () =
    for i = 0 to List.length worldMap - 1 do
      match List.nth worldMap i with
      | ( _,
          {
            Map.name = terr;
            player = x;
            troops = _;
            neighbours = _;
            continent = _;
          } ) -> if x = Some p then terrs.(i) <- Some terr
    done
  in
  terrs

let player_has_territories player =
  let terrs = owned_by player in
  Array.exists Option.is_some terrs

let troops_from_cont player =
  let troops = ref 0 in
  let conts = ref [] in
  let () =
    for i = 0 to List.length worldMap - 1 do
      let country = snd (List.nth worldMap i) in
      if country.player <> Some player then conts := country.continent :: !conts
    done
  in
  if not (List.mem "Asia" !conts) then troops := !troops + 7;
  if not (List.mem "Europe" !conts) then troops := !troops + 5;
  if not (List.mem "North America" !conts) then troops := !troops + 5;
  if not (List.mem "South America" !conts) then troops := !troops + 2;
  if not (List.mem "Africa" !conts) then troops := !troops + 3;
  if not (List.mem "Australia" !conts) then troops := !troops + 2;
  !troops

let num_terrs_owned player =
  let terrs = owned_by player in
  let num_terrs = ref 0 in
  let () =
    for i = 0 to Array.length terrs - 1 do
      match terrs.(i) with
      | Some _ -> num_terrs := 1 + !num_terrs
      | None -> ()
    done
  in
  !num_terrs

let troops_on_start player =
  let troops = troops_from_cont player in
  let num_terrs = num_terrs_owned player in
  if num_terrs <= 11 then troops + 3
  else if num_terrs <= 14 then troops + 4
  else troops + 5

let rec valid_num_troops num_troops name =
  try
    let () =
      print_endline
        ("How many troops would you like to place in " ^ name ^ " : ")
    in
    let input = read_line () in
    let x = int_of_string input in
    if x <= num_troops && x >= 0 then x else raise (Failure "invalid_input")
  with _ ->
    let () = print_endline "Please enter a valid number of troops" in
    valid_num_troops num_troops name

let rec place_troops player num_troops =
  let troops = ref num_troops in
  let terrs = owned_by player in
  let () =
    for i = 0 to List.length worldMap - 1 do
      match terrs.(i) with
      | Some name ->
          if !troops > 0 then
            let () =
              print_endline
                ("You have " ^ string_of_int !troops ^ " troops remaining.")
            in
            let input = valid_num_troops !troops name in
            let terr = List.assoc name worldMap in
            terr.troops <-
              (match terr.troops with
              | Some x ->
                  troops := !troops - input;
                  Some (x + input)
              | None ->
                  troops := !troops - input;
                  Some input)
      | None -> ()
    done
  in
  if !troops > 0 then place_troops player !troops

let rec get_valid_country prompt player is_attacker =
  print_endline prompt;
  let input = read_line () in
  try
    let country = List.assoc input worldMap in
    match country with
    | { Map.player = Some owner; troops = Some troops; _ }
      when is_attacker && owner = player && troops > 1 -> country
    | { Map.player = Some owner; _ } when (not is_attacker) && owner <> player
      -> country
    | _ -> raise Not_found
  with Not_found ->
    print_endline "Invalid country. Please enter a valid country.";
    get_valid_country prompt player is_attacker

let is_reachable country1 country2 =
  let connected = ref false in
  if not (country1.player = country2.player) then false
  else if country1.name = country2.name then false
  else
    let path = ref ("Path: " ^ country1.name) in
    let visited = ref [] in
    let q = Queue.create () in
    let () =
      let () = Queue.push country1 q in
      visited := country1.name :: !visited
    in
    let () =
      try
        while not (Queue.is_empty q) do
          let source = Queue.pop q in
          for i = 0 to List.length source.neighbours - 1 do
            let neigh = List.nth source.neighbours i in
            (if isNeighbour neigh country2 && neigh.player = country1.player
             then
               let () = path := !path ^ " " ^ neigh.name in
               raise (Failure "Connected"));
            if not (List.mem neigh.name !visited) then begin
              visited := neigh.name :: !visited;
              if neigh.player = country1.player then
                let () = path := !path ^ " " ^ neigh.name in
                Queue.push neigh q
            end
          done
        done
      with _ -> connected := true
    in
    !connected

(*[get_int] is a helper function to get a valid integer input*)
let rec get_int () =
  let x =
    try
      let troops = read_line () in
      int_of_string troops
    with _ -> get_int ()
  in
  x

(* Helper function to get a valid number of dice to use *)
let rec get_num_dice prompt max_dice =
  print_endline prompt;
  let input = read_line () in
  try
    let num_dice = int_of_string input in
    if num_dice > 0 && num_dice <= max_dice then num_dice
    else begin
      print_endline "Invalid number of dice. Please try again.";
      get_num_dice prompt max_dice
    end
  with Failure _ ->
    (* Catching failure from int_of_string *)
    print_endline "Please enter a valid number.";
    get_num_dice prompt max_dice

(* Simulate dice rolls and return results in descending order *)
let roll_dice num_dice =
  let rec roll acc n =
    if n = 0 then acc else roll ((Random.int 6 + 1) :: acc) (n - 1)
  in
  List.sort compare (roll [] num_dice) |> List.rev

(* Compare dice rolls and adjust troop numbers *)
let rec compare_rolls attacker defender attack_troops defend_troops =
  match (attacker, defender) with
  | a :: at, d :: dt ->
      if a > d then defend_troops := !defend_troops - 1
      else attack_troops := !attack_troops - 1;
      compare_rolls at dt attack_troops defend_troops
  | _, _ -> ()

let rec move_troops c1 c2 =
  match c1.troops with
  | Some x ->
      let () =
        print_endline
          ("You have " ^ string_of_int x ^ " troops in " ^ c1.name
         ^ ". How many troops would you like to move: ")
      in
      let num_troops = get_int () in
      if num_troops < x then
        let () = c1.troops <- Some (x - num_troops) in
        match c2.troops with
        | Some y -> c2.troops <- Some (y + num_troops)
        | None -> failwith "Precondition Violated"
      else
        let () = print_endline "Please enter a valid number of troops: " in
        move_troops c1 c2
  | None -> failwith "Precondition Violated"

let attack (player : int) =
  (*let () = print_endline "Where would you like to place reinforcements?" in
    place_troops player (troops_on_start player);*)
  let attacking_country : country =
    get_valid_country "Enter the country you want to attack from: " player true
  in
  let defending_country : country =
    get_valid_country "Enter the country you want to attack: " player false
  in

  if not (List.mem defending_country attacking_country.neighbours) then
    failwith "You can only attack neighboring countries";

  let attack_troops = ref (Option.get attacking_country.troops - 1) in
  let defend_troops = ref (Option.get defending_country.troops) in

  let rec loop () =
    if !attack_troops > 0 && !defend_troops > 0 then (
      let attacker_dice =
        get_num_dice "How many dice will the attacker roll? (up to 3)"
          (min 3 !attack_troops)
      in
      let defender_dice =
        get_num_dice "How many dice will the defender roll? (up to 2)"
          (min 2 !defend_troops)
      in

      let attacker_rolls = roll_dice attacker_dice in
      let defender_rolls = roll_dice defender_dice in

      compare_rolls attacker_rolls defender_rolls attack_troops defend_troops;

      print_endline ("Attacker troops left: " ^ string_of_int !attack_troops);
      print_endline ("Defender troops left: " ^ string_of_int !defend_troops);

      loop ())
    else ()
  in
  loop ();

  if !defend_troops = 0 then begin
    print_endline ("Attacker has won " ^ defending_country.name);
    defending_country.player <- Some player;
    defending_country.troops <- Some 1;
    attacking_country.troops <- Some (!attack_troops - 1)
  end
  else print_endline ("Defender has held " ^ defending_country.name)

let rec fortify player =
  try
    let () =
      print_endline
        "What country would you like to fortify from (If none, type none): "
    in
    let input1 = read_line () in
    if input1 = "none" then ()
    else if List.mem input1 countries then
      let () = print_endline "What country would you like to fortify to: " in
      let input2 = read_line () in
      if List.mem input2 countries then
        let c1 = List.assoc input1 worldMap in
        let c2 = List.assoc input2 worldMap in
        match c1.troops with
        | Some x ->
            if Some player = c1.player && x > 1 && is_reachable c1 c2 then
              move_troops c1 c2
            else failwith "Invalid Argument"
        | None -> failwith "Precondition Violated"
      else failwith "Invalid Argument"
    else failwith "Invalid Argument"
  with _ ->
    let () = print_endline "Please enter a valid country! " in
    fortify player
