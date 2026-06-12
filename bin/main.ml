
open Monitoring_lib
open Types
open Print_functions


let _agents = AgentSet.of_list["a"; "b";"c"(*;"d";"e";"f"*)]

(* Formules*)
(*
let _formule = Tformula.Exists("a",Not(Top))
let _star = Tformula.Exists("a_",
                     Forall ("b_", And (
                     Leq(Ag "b_", Diam (0.,2.,Ag ("a_"))),
                     Implies (Not(Eq(Ag "a", Ag "b")), 
                     Leq(Diam(0.,1.,Ag "b"),Or(Ag "a", Ag "b"))
                     )
                     ))
                     )
let _nul = Tformula.Not(Top)


let _test1= Tformula.Exists("a_",
    Exists("b_",
        Leq(Ag "a_", F(0.,1.,Diam (0.,1., Ag "b_") 
        ))))

let _testDiam = Tformula.Leq(Ag "a", Diam (0.,1., Ag "b") )
let _testF = Tformula.Leq(Ag "a", F(1.,2., Diam (0.,1., Ag "b") ))
let _testMu = Tformula.Leq(Ag "a", Mu ("x", Or (Ag "b",Diam (0.,1., Var "x"))) )
let _testMuF = Tformula.Leq(Ag "a", H(1.,Mu ("x", Or (Ag "b",F(0.,2.,Diam (0.,1., Var "x")))) ))*)
(*Mise en place du signal*)

let _a0 = AgentMap.of_seq @@ List.to_seq [
("a",(0.,0.));
("b", (0.,1.));
("c", (0.,5.));

]

let _a1 = AgentMap.of_seq @@ List.to_seq [
("a",(0.,0.));
("b", (0.,4.));
("c", (0.,5.));
]

let _a2 = AgentMap.of_seq @@ List.to_seq [
("a",(0.,1.));
("b", (0.,0.));
("c", (0.,4.));
("d", (0.,3.));
("e", (0.,4.));
("f", (0.,5.));
]

let _aa = AgentMap.of_seq @@ List.to_seq [
("a",(0.,0.));
("b", (0.,1.));
("c", (0.,5.));
("d", (0.,3.));
("e", (0.,4.));
("f", (0.,5.));
]

let _ex_sig = FloatMap.of_seq @@ List.to_seq [
(0., _aa);
(*(1., _a1);
(2., _a2);*)
]

let _mu_cache = ref(FloatMap.map (fun _ -> FloatMap.of_seq @@ List.to_seq [(0.,exactly (AgentSet.singleton "b"));])_ex_sig)


let _mu_cache = ref (empty_tSigA _ex_sig)


(*let _res = Tformula.bSem _testMuF _agents _ex_sig mu_cache

let () =
  print_endline (string_of_signal_signal _res)*)



let _test_sh = Af_sh.Mu("x", Or (Ag "a",Diam(0.,3.,Not(Diam (0.,3.,Not( Var "x"))))))
let _test_sh = Af_sh.Mu("x",Or(Ag "a",And(Diam(0.,2.,Not(Diam(0.,2.,Ag "b"))), Diam (0.,1., Var "x"))))
let _test_sh = Af_sh.Diam(0.,1.,Not(Diam(0.,1.,Ag"a")))
let _test_sh = Af_sh.Diam(0.,1.,And(Diam(0.,1.,Ag "b"), Or(
  Not(Diam(0.,1.,Diam(0.,1.,Ag"c"))),
  Diam(0.,1.,Diam(0.,1.,Diam(0.,1.,Ag "d")))
)))
let _test_sh = Af_sh.Mu("x", Or (Ag "a",Diam(1.,1., Var "x")))

let _test_sh = Af_sh.Diam(0.,1.,Top)

(*
let test_sh = Af_sh.Mu("x", Or (Ag "a",Diam (0.,1., Var "x")))



let test_sh = Af_sh.Diam(0.,1.,Ag "c")
let test_sh = Af_sh.Mu("x", Or (Ag "a",(Diam(0.5,1.,Not(Diam (0.5,1.,Not( Var "x")))))))


let test_sh = Af_sh.Diam(0.,1.,And(Diam(0.,1.,Ag "b"), Or(
  Not(Diam(0.,1.,Diam(0.,1.,Ag"c"))),
  Diam(0.,1.,Diam(0.,1.,Diam(0.,1.,Ag "d")))
)))*)
(*let truc = Af_sh.setSem test_sh _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D

let () = print_endline ("Result :")
let result = string_of_string_set_between_signal_sh truc
let () = print_endline (result)*)


let rec _write_sem folder l = match l with
  | (filename,sem) :: ll -> 
    let oc = open_out (folder ^ "/" ^ filename) in
    let result = string_of_string_set_between_signal_sh sem in
    Printf.fprintf oc "%s\n" result;
    close_out oc;
    _write_sem folder ll;
  | [] -> () 


let () = _write_sem "ex_output/new_test" Ex_pres.formula_list
let () = _write_sem "ex_output/new_test" Ex_jeremy.formula_list
let () = _write_sem "ex_output/new_test" Abc1.formula_list 

