open Types

(* let read_csv filename agents =
  let csv_path =
  Filename.concat (Sys.getenv "PWD") filename in
  let ic = open_in csv_path in
  let _ = input_line ic in
  let rec loop map =
    try
      let line = input_line ic in
      let fields = String.split_on_char ',' line in
      let ts = float_of_string (List.hd fields) in
      let rec loop2 amap ag am =
        (match (amap,ag) with
        | ([], []) -> 
          print_endline "Oh";
          am
        | (p::l, a::ap) ->
          Printf.printf "Hey: p=[%s]\n%!" p;
          loop2 l ap (AgentMap.add a (Scanf.sscanf p "\"(%f,%f)\"" (fun x y -> (x, y))) am);
        | _ -> failwith ("Number of data does not coincide with number of agents"))
      in
      loop (FloatMap.add ts (loop2 (List.tl fields) agents AgentMap.empty) map)
    with
    | End_of_file ->
        print_endline "Lets go";
        close_in ic;
        map
  in
  loop FloatMap.empty *)

let read_csv filename agents timeout =
  let csv_path = filename
    (* Filename.concat (Sys.getenv "PWD") filename *)
  in

  let rows =
    match Csv.load csv_path with
      | [] -> []
      | _header :: data -> data
  in 

  let rec go l i =
    match l with 
    | _ when i == -1 -> l
    | _header :: data when i > 0 -> _header::(go data (i-1))
    | _ -> []
  in
  let rows = go rows timeout in

  let parse_agent_map fields =
    let rec aux fields agents acc =
      match fields, agents with
      | [], [] ->
          acc
      | p :: ps, a :: ags ->
          let value =
            Scanf.sscanf p "(%f, %f)" (fun x y -> (x, y))
          in
          aux ps ags (AgentMap.add a value acc)
      | _ ->
          failwith "Number of data does not coincide with number of agents"
    in
    aux fields agents AgentMap.empty
  in

  List.fold_left
    (fun (fmap,i) row ->
      match row with
      | [] ->
          (fmap,i)
      | ts_str :: fields ->
          let ts = (float_of_string ts_str)-.1. in
          let amap = parse_agent_map fields in
          (FloatMap.add ts amap fmap,i+1))
    (FloatMap.empty,0)
    rows