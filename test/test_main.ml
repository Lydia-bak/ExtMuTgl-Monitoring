

open Monitoring_lib
open Types
open Print_functions
(* open Input *)

(*Set of agents*)
let _agents = AgentSet.of_list["a"; "b";"c";"d";"e";"f"]
let pond_func = Pond.euc_dist_2D
(* 
let _agents2 = AgentSet.of_list["a"; "b";"c";"d";"e";"f";"g";"h";"i";"j"]
let _tgr = read_csv "data.csv" ["a"; "b";"c";"d";"e";"f";"g";"h";"i";"j"]

let () = match FloatMap.min_binding_opt _tgr with
  | None ->
      print_endline "FloatMap is empty"
  | Some (fkey, smap) ->
      Printf.printf "First key: %f\n" fkey;
      AgentMap.iter
        (fun skey (x, y) ->
          Printf.printf "  %s -> (%f, %f)\n" skey x y)
        smap

let mu_cache = ref (empty_tSigA _tgr) *)

(* let _testMuFT = Tf_sh.Leq(Ag "f", H(3.,Mu ("x", Or (Ag "b",F(0.,2.,Diam (0.,1., Var "x")))) )) *)
(* let _resMuFT = Tf_sh.bSem _testMuFT _agents2 _tgr mu_cache pond_func *)


(* ------------------ *)


(* Signal *)
let _a0 = AgentMap.of_seq @@ List.to_seq [
("a",(0.,4.));
("b", (0.,0.));
("c", (0.,5.));
("d", (0.,6.));
("e", (0.,8.));
("f", (0.,9.));

]

let _a1 = AgentMap.of_seq @@ List.to_seq [
("a",(0.,4.));
("b", (0.,0.));
("c", (0.,5.));
("d", (0.,6.));
("e", (0.,7.));
("f", (0.,9.));
]

let _a2 = AgentMap.of_seq @@ List.to_seq [
("a",(0.,1.));
("b", (0.,0.));
("c", (0.,4.));
("d", (0.,3.));
("e", (0.,4.));
("f", (0.,5.));
]

let _ex_sig = FloatMap.of_seq @@ List.to_seq [
(0., _a0);
(1., _a1);
(2., _a2);
]


let mu_cache1 = ref (empty_tSigA _ex_sig)
let mu_cache2 = ref (empty_tSigA _ex_sig)

(* ------------------ *)


(*Truth Formulas*)
let _testDiamT = Tf_sh.Leq(Ag "a", Diam (0.,1., Ag "b") ) 
let _testFT = Tf_sh.Leq(Ag "a", F(0.,1.,Diam (0.,1., Ag "b") ))
let _testMuT = Tf_sh.Leq(Ag "b", Mu ("x", Or (Ag "b",Diam (0.,1., Var "x"))) )
let _testMuFT = Tf_sh.Leq(Ag "f", H(3.,Mu ("x", Or (Ag "b",F(0.,2.,Diam (0.,1., Var "x")))) ))

(*Truth Formulas*)
let _testDiamA = Af_sh.Diam (0.,1., Ag "b")
let _testFA = Af_sh.F(0.,1.,Diam (0.,1., Ag "b"))
let _testMuA = Af_sh.SHor (5.0,Af_sh.Mu ("x", Or (Ag "b",F(0.,2.,Diam (0.,1., Var "x")))))


(* ------------------ *)


(*Semantic results*)

let _resDiamT = Tf_sh.bSem _testDiamT _agents _ex_sig mu_cache2 pond_func
let _resFT = Tf_sh.bSem _testFT _agents _ex_sig mu_cache2 pond_func
(*let _resMuT = Tf_sh.bSem _testMuT _agents _ex_sig mu_cache*)
let _resMuFT = Tf_sh.bSem _testMuFT _agents _ex_sig mu_cache1 pond_func

let _resDiamA = Af_sh.setSem _testDiamA _agents _ex_sig mu_cache2 Float.infinity pond_func
let _resFA = Af_sh.setSem _testFA _agents _ex_sig mu_cache2 Float.infinity pond_func
let _resMuA = Af_sh.setSem _testMuA _agents _ex_sig mu_cache2 2. pond_func

let () = print_endline (string_of_string_set_between_signal_sh _resMuA)


let _semresMuFT = 
  FloatMap.(empty
    |> add 0.0 FloatMap.(empty
      |> add 0.0 Bool3.Undet
      |> add 1.0 Bool3.Undet
      |> add 2.0 Bool3.Top
    )
    |> add 1.0 FloatMap.(empty
      |> add 0.0 Bool3.Undet
      |> add 1.0 Bool3.Bot
    )
    |> add 2.0 FloatMap.(empty
      |> add 0.0 Bool3.Bot
    )
  )

  (* let () = assert (FloatMap.equal (FloatMap.equal Bool3.equal) _resMuFT _semresMuFT) *)