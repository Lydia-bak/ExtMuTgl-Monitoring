open Monitoring_lib
open Types

let _agents = AgentSet.of_list["a"; "b";"c"]

let _aa = AgentMap.of_seq @@ List.to_seq [
("a",(0.,0.));
("b", (0.,1.));
("c", (0.,5.));
]
let _ex_sig = FloatMap.of_seq @@ List.to_seq [
(0., _aa);
]

let formula_1 = Af_sh.Mu("x",Or(Not(Diam (0.,2.,Ag "a")),Diam (0.,2., Var "x")))
let formula_2 = Af_sh.Mu("x", Or (Ag "a",Not(Diam(1.,2.,Not(Diam (1.,2., Var "x"))))))
let formula_3 = Af_sh.Mu("x",Or(Not(Diam (0.,1.,Ag "a")),Diam (0.,1., Var "x")))

let mu_cache = ref (empty_tSigA _ex_sig)
let sem1 = Af_sh.setSem formula_1 _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D
let mu_cache = ref (empty_tSigA _ex_sig)
let sem2 = Af_sh.setSem formula_2 _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D
let mu_cache = ref (empty_tSigA _ex_sig)
let sem3 = Af_sh.setSem formula_3 _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D

let formula_list = [("mu_not_diam02_a_or_diam02_x",sem1) ; ("mu_a_or_not_diam12_not_diam12_x",sem2) ; ("mu_not_diam01_a_or_diam01_x",sem3)]