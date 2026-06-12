(*Functions used for computing F operator *)

open Types

let neighbours_from_aset aSet tGraph d1 d2 u v =
  let u' = AgentSet.filter (
    fun a -> AgentSet.exists(fun b -> (
      distance (AgentMap.find(a) tGraph)(AgentMap.find(b) tGraph) >= d1
      ) && (distance(AgentMap.find(a) tGraph)(AgentMap.find(b) tGraph) <= d2)) u
    ) aSet in
  Between (u', AgentSet.(union v u') )

let update_after_shift  smap1 hmap h=
  let (_,smap2) = FloatMap.find_last(fun h' -> h'<= h ) hmap in
  (*if (h+.t) < (t+.b) then Some(merge_smap smap1 smap2)
  else if (h+.t) = (t+.b) then Some(
    merge_sig_end b_union_u b_union smap1 smap2
  )
  else Some(merge_sig_end b_union_u b_union smap1 smap2)*)
  Some(merge_smap smap1 smap2)

let _shift s hmap aSet = match s with 
  |0. -> FloatMap.update 0. (function  
    |Some smap -> Some (FloatMap.map( fun (Between(u,_)) -> Between(u,aSet)
    ) smap) 
    |None -> None
  )hmap
  |_ -> let res = FloatMap.of_seq @@ List.to_seq [(0.,singleton_sSig  0. (Between(AgentSet.empty, aSet)));] in
    
    FloatMap.fold(fun h smap acc -> 
      FloatMap.add (h+.s) smap acc
  )hmap res

let merge_shift_by_hor shift_hmap acc smap1 smap2 =
  match (smap1, smap2) with
    (*| (Some(sm1), Some(sm2)) -> 
      fun _ _ _ -> Some(merge_smap sm1 sm2)*)
    | (_,Some(sm2)) -> 
      update_after_shift sm2 shift_hmap
    | (Some(sm1),None) -> 
      update_after_shift sm1 acc
    |(None,None) -> fun _ -> None

  (* new functions here  *)
let rm_redundant_h_key res = 
  FloatMap.iter(fun t hmap ->
    let resh = ref hmap in
    FloatMap.iter(fun h smap ->
      match FloatMap.find_last_opt(fun h' -> h' < h) !resh with
        |Some(_,smap') -> 
          if FloatMap.equal ( fun (Between(ux,vx)) (Between(uy,vy)) -> 
              AgentSet.equal ux uy && AgentSet.equal vx vy) smap smap' then
            resh := FloatMap.remove h !resh
        |None -> ()
    ) !resh;
    res := FloatMap.remove t !res;
    res := FloatMap.add t !resh !res
    ) !res

let rm_big_key_hor res ss = 
  FloatMap.iter(fun s _ ->
    if s >= ss then
      res := FloatMap.remove s !res;
  ) !res