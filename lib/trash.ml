 (*let _construct_partial_Smaps_sup mm d _ag ag' _hmap=
    (*FloatMap.iter ( fun s (Between (u,_v)) ->
    if (AgentSet.mem ag u && not(AgentSet.is_empty u)) then (
      mm := FloatMap.update (s) (fun uset -> match uset with
      |Some (Between(u',v')) -> Some(Between(u',AgentSet.add ag' v'))
      |None -> Some(Between(AgentSet.empty,AgentSet.singleton ag'))
    ) !mm;
      mm := FloatMap.update (s+.d) (fun uset -> match uset with
        |Some (Between(u',v')) -> Some(Between(AgentSet.remove ag' u',AgentSet.remove ag' v'))
        |None -> Some(exactly(AgentSet.empty))
      )!mm;)
    )*)
    let found = ref false in
    let ss = ref 0. in
    FloatMap.iter ( fun s (Between (u,_v)) -> (
      if !found = false then (
        if AgentSet.mem _ag u then (
          mm := FloatMap.update (s) (fun uset -> match uset with
            |Some (Between(u',v')) -> Some(Between(u',AgentSet.add ag' v'))
            |None -> Some(Between(AgentSet.empty,AgentSet.singleton ag'))
          ) !mm;
          mm := FloatMap.update (s+.d) (fun uset -> match uset with
            |Some (Between(u',v')) -> Some(Between(AgentSet.remove ag' u',AgentSet.remove ag' v'))
            |None -> Some(exactly(AgentSet.empty))
          )!mm;
          found := true;
          ss := s;
        ))
      else( if s > !ss+.d then 
        (mm := FloatMap.update s (fun uset -> match uset with
            |Some (Between(u',v')) -> Some(Between(AgentSet.remove ag' u',AgentSet.remove ag' v'))
            |None -> Some(exactly(AgentSet.empty))
          )!mm;)
          else 
            mm := FloatMap.update s (fun uset -> match uset with
            |Some (Between(u',v')) -> Some(Between(AgentSet.remove ag' u',AgentSet.add ag' v'))
            |None -> Some(exactly(AgentSet.empty))
          )!mm;))

    ) _hmap*)