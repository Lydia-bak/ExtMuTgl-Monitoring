open Monitoring_lib
open Types

let _agents = AgentSet.of_list["a"; "b";"c";"d"]

let _aa = AgentMap.of_seq @@ List.to_seq [
("a",(0.,0.));
("b", (0.,1.));
("c", (0.,2.));
("d", (0.,3.));
]
let _ex_sig = FloatMap.of_seq @@ List.to_seq [
(0., _aa);
]

let formula_1 = Af_sh.And(Diam(0.,1.,Ag "b"), Or(
  Not(Diam(0.,1.,Diam(0.,1.,Ag"c"))),
  Diam(0.,1.,Diam(0.,1.,Diam(0.,1.,Ag "d")))
))

let formula_2 = Af_sh.Diam(0.,1.,And(Diam(0.,1.,Ag "b"), Or(
  Not(Diam(0.,1.,Diam(0.,1.,Ag"c"))),
  Diam(0.,1.,Diam(0.,1.,Diam(0.,1.,Ag "d")))
)))

let mu_cache = ref (empty_tSigA _ex_sig)
let sem1 = Af_sh.setSem formula_1 _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D
let mu_cache = ref (empty_tSigA _ex_sig)
let sem2 = Af_sh.setSem formula_2 _agents _ex_sig mu_cache Float.infinity Pond.euc_dist_2D



let formula_list = [("jeremy_ex",sem1) ; ("diam01_jeremy_ex",sem2)]