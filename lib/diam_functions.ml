(*Functions used for computing diamond operator *)
open Types

(*print functions by chat gpt*)
let print_agentset s =
  let agents = AgentSet.elements s in
  print_string "{";
  List.iter (fun a ->
    Printf.printf "%s " a
  ) agents;
  print_string "}"

let print_between_list lst =
  List.iter (fun (f, Between (s1, s2)) ->
    Printf.printf "(%f, Between (" f;
    print_agentset s1;
    print_string ", ";
    print_agentset s2;
    print_endline "))"
  ) lst;
  print_endline "..."

let rec check lhmap lres (d,(ag,ag')) =
  match lhmap with 
    |(s,Between(u,v))::llhmap -> 
      if AgentSet.mem ag u then (
          if d+.s = 0. then (0.,Between(AgentSet.singleton ag', AgentSet.singleton ag'))::[]
          else (
            List.rev ((s+.d,Between(AgentSet.singleton ag', AgentSet.singleton ag'))::lres)
          )
      )
      else(
        if (AgentSet.mem ag v) then check llhmap lres (d,(ag, ag'))
        else (
          if d+.s = 0. then [(0., Between(AgentSet.empty,AgentSet.empty))]
          else List.rev ((s+.d, Between(AgentSet.empty,AgentSet.empty))::lres)
        )
      )
    |_ -> List.rev lres



let rec construct lhmap lres ledges =
  match ledges with 
    | (d,(ag,ag'))::lledges ->
      ( let tmp = (check lhmap [(0.,Between(AgentSet.empty, AgentSet.singleton ag'))] (d,(ag,ag'))) in
    let _newtmp = tmp @ (check [List.hd (List.rev lhmap)] [] (d,(ag,ag'))) in
    construct lhmap ((_newtmp)::lres) lledges
      
    )
    |_ -> lres

let rec construct_rest lres ledges =
  match ledges with 
    | (d,(_,ag'))::lledges ->
      let tmp =  ([(0.,Between(AgentSet.empty, AgentSet.singleton ag')); (d,Between(AgentSet.empty, AgentSet.empty)); (infinity,Between(AgentSet.empty, AgentSet.empty))])  in
      construct_rest (tmp::lres) lledges
    |_ -> lres



let rec finish_union lacc uSet = match lacc with
  |(s,uSetacc)::llacc -> (s, b_union uSet uSetacc):: finish_union llacc uSet 
  |_ -> lacc

let rec unionrec ledge lacc uSet_e uSet_acc =
  match (ledge, lacc) with
    |((se, uSete)::_gne,(sacc, uSetacc)::llacc) -> (
      if se=sacc then 
          (se, b_union uSete uSetacc)::finish_union llacc uSete
      else if se > sacc then 
          (sacc,b_union uSet_e uSetacc)::unionrec ledge llacc uSet_e uSetacc
      else
          (se,b_union uSete uSet_acc):: (sacc,b_union uSete uSetacc):: finish_union llacc uSete
    )
  |([],lacc) -> finish_union lacc uSet_e
  |((infinity, uSete)::_ll,_) -> [(infinity, b_union uSete uSet_acc)]
  (*|_,_ -> print_endline("error !!???"); 
      print_between_list ledge;
      print_between_list lacc;
      []*)


let union_begin ledges lacc = 
    match (ledges,lacc) with 
    |((0.,uSete)::lledge, (0.,uSetacc)::llacc) -> 
      (0.,b_union uSete uSetacc)::(unionrec lledge (llacc) uSete uSetacc)
    |(_,_) -> print_endline("error : no 0 at the beginning of the list ! "); 
          []


let rec  merge_edges_sem l lres = 
  match l with 
    |lpart::ll -> merge_edges_sem ll (union_begin ( lpart) lres)
    |_ -> lres

let _get_list_edges aSet tGraph d1 d2 pond_func =
  let tmp = ref [] in
  let tmp_inf = ref [] in
  let tmp_sup = ref [] in
  AgentSet.iter (fun ag -> 
    AgentSet.iter (fun ag' -> 
      let coord_ag = AgentMap.find(ag) tGraph in
      let coord_ag' = AgentMap.find(ag') tGraph in
      (* let d = distance (coord_ag )(coord_ag' ) in *)
      let ponderated_d = pond_func (ag, coord_ag) (ag',coord_ag') in
      if ((ponderated_d >= d1) && (ponderated_d <= d2)) then
        tmp := (ponderated_d ,(ag,ag'))::!tmp;
      if (ponderated_d < d1) then 
        tmp_inf := (ponderated_d ,(ag,ag'))::!tmp_inf;
      if (ponderated_d > d2) then tmp_sup := (d2 ,(ag,ag'))::!tmp_sup;
    ) aSet
  ) aSet;
  (!tmp,!tmp_inf,!tmp_sup)

let _construct_partial_Slists ledges lhmap = construct lhmap [] ledges

let _construct_partial_Slist_rest ledges = construct_rest [] ledges

let merge_all_edges_sem lsem1 lsem2 lsem3 =
  merge_edges_sem lsem1 (merge_edges_sem lsem2 (merge_edges_sem lsem3 [(0.,Between(AgentSet.empty, AgentSet.empty))]))

let rm_big_key res ss = 
  let (_,infvalue) = FloatMap.find_last (fun _ -> true) !res in
  FloatMap.iter(fun s _ ->
    if s > ss (*&& s != infinity*) then
      res := FloatMap.remove s !res;
  ) !res;
  res := FloatMap.add infinity infvalue !res