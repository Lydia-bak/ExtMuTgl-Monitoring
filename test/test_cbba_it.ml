open Monitoring_lib
open Types
open Input

let ag_sizes = [3;5;10;20;50]
let nb_it = 10
let rec go i =
  match i with
  |0 -> []
  |_ -> (string_of_int i)::(go (i-1))
let pond_func = Pond.hops2 100. 

let filename = "../../../experiment2.csv"
let append_csv row =
  let oc = Csv.to_channel
    (open_out_gen
      [Open_creat; Open_text; Open_append]
      0o666
      filename)
  in
  Csv.output_record oc row;
  Csv.close_out oc

let find_csv () =
  let files = Sys.readdir (Sys.getcwd ()) in
  let csvs =
    Array.to_list files
    |> List.filter (fun f -> Filename.check_suffix f ".csv")
  in
  match csvs with
  | [f] -> f
  | [] -> failwith "No CSV file found"
  | _ -> failwith "Multiple CSV files found"

let cwd = Sys.getcwd ()
let space_sim = "/Users/jeremydubut/Documents/github/space-simulator"

let it nb_agent =
  let () = ignore(Sys.command ("cd "^space_sim^" && yq -i \".agents.quantity="^(string_of_int nb_agent)^"\" config.yaml && yq -i \".tasks.quantity="^(string_of_int (3*nb_agent))^"\" config.yaml")) in
  for _ = 1 to nb_it do
    (* let _ = Unix.chdir space_sim in
    let _ = Sys.command "conda init" in
    let _ = Sys.command "conda activate rv26" in
    let _ = Sys.command "python3 main.py" in 
    let _ = Unix.chdir cwd in *)
    let () = ignore(Sys.command ("cd "^space_sim^" && conda run -n rv26 python3 main.py > /dev/null 2>&1")) in
    let f = find_csv() in
    let _agents_list = go nb_agent in
    let _agents = AgentSet.of_list _agents_list in 
    let _tgr,size = read_csv (Filename.concat cwd  f) _agents_list (-1) in
    let () = ignore (Sys.command ("rm "^f)) in
    let mu_cache = ref (empty_tSigA _tgr) in
    let ifl = float_of_int nb_agent in 
    let diamBound = 
      (* Tf_sh.Forall ("ag",  *)
        Tf_sh.Leq (
          Af_sh.Top,
          Af_sh.SHor(ifl/.2., 
            Af_sh.H(20.,
              Af_sh.Mu("x",
                Af_sh.Or(
                  Ag "1",
                  F(0.,20.,
                    Diam(0.,ifl/.2.,
                      Af_sh.Var "x"
                    )
                  )
                )
              )
            )
          )
        ) in
      (* ) *)
    let start = Sys.time () in
    let _ = Tf_sh.bSem diamBound _agents _tgr mu_cache pond_func in 
    let elapsed = Sys.time () -. start in
    let row = [string_of_int nb_agent; string_of_float elapsed; string_of_int size] in
    append_csv row
  done  

let () =
  List.fold_left (fun _ i -> it i) () ag_sizes