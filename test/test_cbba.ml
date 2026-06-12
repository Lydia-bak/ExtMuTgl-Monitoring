open Monitoring_lib
open Types
(* open Print_functions *)
open Input
(* open Csv *)

let output = "../../../experiment1.csv"
let () = close_out (open_out output)
let append_csv row =
  let oc = Csv.to_channel
    (open_out_gen
      [Open_creat; Open_text; Open_append]
      0o666
      output)
  in
  Csv.output_record oc row;
  Csv.close_out oc

let nb_agent = 10
let rec go i =
  match i with
  |0 -> []
  |_ -> (string_of_int i)::(go (i-1))
let _agents_list = go nb_agent
let _agents = AgentSet.of_list _agents_list
let filename = Filename.concat (Sys.getenv "PWD") "data.csv"
let _tgr,_ = read_csv filename _agents_list (-1)
let pond_func = Pond.hops2 500.

let () =
for i = 1 to 10 do
  let mu_cache = ref (empty_tSigA _tgr) in
  let ifl = float_of_int i in
  let diamBound = 
    Tf_sh.Forall ("ag", 
      Tf_sh.Leq (
        Af_sh.Top,
        Af_sh.SHor(ifl, 
          Af_sh.H(10.,
            Af_sh.Mu("x",
              Af_sh.Or(
                Ag "ag",
                F(0.,10.,
                  Diam(0.,ifl,
                    Af_sh.Var "x"
                  )
                )
              )
            )
          )
        )
      )
    ) in
  let start = Sys.time () in
  let sem = Tf_sh.bSem diamBound _agents _tgr mu_cache pond_func in 
  let elapsed = Sys.time () -. start in
  let sem' = ref (FloatMap.map 
    (fun hmap -> 
      match FloatMap.find_last_opt (fun _ -> true) hmap with
      |Some (_,b) -> b
      |None -> Bool3.Undet
      ) sem) in
  FloatMap.iter(fun s b ->
        match FloatMap.find_last_opt(fun s' -> s' < s) !sem' with
          |Some(_,b') -> 
            if Bool3.equal b b' then 
              sem' := FloatMap.remove s !sem';
          |None -> ()
      ) !sem';
  (* print_endline (string_of_signal !sem'); *)

  let row = FloatMap.fold (fun t b l -> ("("^(string_of_float t)^","^(Bool3.to_string b)^")")::l) !sem' [] in
  append_csv ((string_of_int i)::((string_of_float elapsed)::(List.rev row)));
  (* print_string "Execution time :";
  print_endline (Float.to_string elapsed) *)
done