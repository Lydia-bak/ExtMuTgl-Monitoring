
open Monitoring_lib
open Types

let _agents = AgentSet.of_list["a"; "b";"c";"d";"e";"f"]

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

let formula_1 = Af_sh.Mu("x", Or (Ag "b",F(0.,2.,Diam (0.,1., Var "x"))))
let formula_2 = Af_sh.Mu("x", Or (Ag "b",F(0.,1.,Diam (0.,1., Var "x"))))


let mu_cache = ref (empty_tSigA _ex_sig)
let sem1 = Af_sh.setSem formula_1 _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D
let mu_cache = ref (empty_tSigA _ex_sig)
let sem2 = Af_sh.setSem formula_2 _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D

let formula_list = [("mu_b_or_f02_diam01_x",sem1) ; ("mu_b_or_f01_diam01_x",sem2)]