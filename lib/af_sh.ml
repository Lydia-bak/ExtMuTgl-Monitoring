(** Module Aformula for space horizon
  * specifies the form of the agent formulas along with their semantic.
*)

open Types
open Diam_functions
open F_functions


type t = 
    | Top
    | Ag of agent
    | Not of t
    | And of t * t
    | Or of t * t
    | Implies of t * t 
    | Exists of agent * t
    | Forall of agent * t
    | F of float * float * t
    | Diam of float * float * t
    | H of float * t
    | Mu of fpv * t
    | Var of fpv
    | SHor of float * t

  

  let union_sh (x: uncertainSet signal) (y: uncertainSet signal) =
    FloatMap.merge( fun sh uSetx uSety -> match uSetx, uSety with
      |Some(uSetxx),Some(uSetyy) -> Some(b_union uSetxx uSetyy)
      |Some(uSetxx), None -> 
                            (*let(_,uSetyy) = FloatMap.find_last(fun sh' -> sh'<= sh ) y in CHANGER ?*)
                            let uSetyy = find_last_or_sem sh y uSetxx in
                            Some(b_union uSetxx uSetyy)
      |None, Some(uSetyy) -> 
                            (*let(_,uSetxx) = FloatMap.find_last(fun sh' -> sh'<= sh ) x in *)
                            let uSetxx = find_last_or_sem sh x uSetyy in
                            Some(b_union uSetxx uSetyy)
      |_,_ -> None
    )x y


  let union (x: uncertainSet signal signal signal) (y: uncertainSet signal signal signal) : uncertainSet signal signal signal = 
    FloatMap.union (fun _ sigx sigy ->
      Some(FloatMap.merge( fun h mapx mapy -> match mapx, mapy  with

        |Some(hmapx),Some(hmapy) -> Some(union_sh hmapx hmapy)
        |Some(hmapx), None -> let(_,hmapy) = FloatMap.find_last(fun h' -> h'<= h ) sigy in 
                              Some(union_sh hmapx hmapy)
        |None, Some(hmapy) -> let(_,hmapx) = FloatMap.find_last(fun h' -> h'<= h ) sigx in 
                              Some(union_sh hmapx hmapy)
        |_,_ -> None
      )sigx sigy
    )) x y

  

  let rm_redundant_key res = 
    let (_,infvalue) = FloatMap.find_last (fun _ -> true) !res in
    FloatMap.iter(fun s uSet ->
      if s != infinity then
      match FloatMap.find_last_opt(fun s' -> s' < s) !res with
        |Some(_,uset) -> 
          if b_is_equal uSet uset then 
            res := FloatMap.remove s !res;
        |None -> ()
    ) !res;
    res := FloatMap.add infinity infvalue !res
        
  (** Substitute the agent variable a with b in a Aformula
    * Used for Exists and Forall operators 
    *)
  let rec setAgentFormula (a) (b) (phi)  : t = match phi with
    | Top -> Top
    | Var x ->  Var x
    | Ag c -> if a = c then Ag b else Ag c
    | Not phi1 -> Not (setAgentFormula a b phi1)
    | Diam (d1, d2, phi1) -> Diam(d1,d2, setAgentFormula a b phi1)
    | Exists (c, phi1) -> Exists(c, setAgentFormula a b phi1)
    | Forall (c, phi1) -> Forall(c, setAgentFormula a b phi1)
    | F (s, t, phi1) -> F(s,t,setAgentFormula a b phi1)
    | H (t, phi1) -> H(t,setAgentFormula a b phi1)
    | And (phi1, phi2) -> And (setAgentFormula a b phi1,setAgentFormula a b phi2)
    | Or (phi1, phi2) -> Or (setAgentFormula a b phi1, setAgentFormula a b phi2)
    | Implies (phi1, phi2) -> Implies (setAgentFormula a b phi1, setAgentFormula a b phi2)
    | Mu (x, phi1) ->  Mu (x,setAgentFormula a b phi1)
    | SHor (s, phi1) -> SHor (s, setAgentFormula a b phi1)

  let rec hSize (phi : t) : float = match phi with
    | Top -> 0.
    | Var _ -> 0.
    | Ag _ -> 0.
    | Not phi1 -> hSize phi1
    | Diam (_, _, phi1) -> hSize phi1
    | Exists (_, phi1) -> hSize phi1
    | Forall (_, phi1) -> hSize phi1
    | F (_, t, phi1) -> hSize phi1 +. t
    | H (t, phi1) -> min t (hSize phi1)
    | And (phi1, phi2) | Or (phi1, phi2) | Implies (phi1, phi2)-> max(hSize phi1) (hSize phi2)
    | Mu (_, phi1) -> if hSize phi1 == 0. then 0. else infinity
    | SHor (_, phi1) -> hSize phi1

    (* new function here *)
  let rec sSize (phi : t) : float = match phi with
    | Top -> 0.
    | Var _ -> 0.
    | Ag _ -> 0.
    | Not phi1 -> sSize phi1
    | Diam (_, _, phi1) -> sSize phi1
    | Exists (_, phi1) -> sSize phi1
    | Forall (_, phi1) -> sSize phi1
    | F (_, _, phi1) -> sSize phi1 
    | H (_, phi1) -> sSize phi1
    | And (phi1, phi2) | Or (phi1, phi2) | Implies (phi1, phi2)-> max (hSize phi1) (hSize phi2)
    | Mu (_, phi1) -> sSize phi1
    | SHor (s, phi1) -> max s (hSize phi1)

  let setSem (phi : t) (aSet : AgentSet.t) (osig : graph signal) (x_: uncertainSet signal signal signal ref) (h_:float) (pond_funct:(agent * coord -> agent * coord -> float)) : uncertainSet signal signal signal = 
    let ss = sSize phi in
    let rec setSemAux (phi : t) h_ : uncertainSet signal signal signal = 
    match phi with

    (* added infinity in the two base cases *)
    |Top -> FloatMap.map (fun _ -> 
      [(0., ([(0.,exactly aSet);(infinity,exactly aSet);] |> List.to_seq |> FloatMap.of_seq));] |> List.to_seq |> FloatMap.of_seq ) osig
 
    |Ag a -> FloatMap.map (fun _ -> 
      [(0., ([(0.,exactly (AgentSet.singleton a));(infinity,exactly (AgentSet.singleton a));] |> List.to_seq |> FloatMap.of_seq));] |> List.to_seq |> FloatMap.of_seq ) osig
    
    |Not phi1 -> 
      let x = setSemAux phi1 h_ in
      let res = FloatMap.map( 
        fun tmap -> (
          FloatMap.map(
            fun hmap -> (
              FloatMap.map(
                fun (Between (u, v)) -> 
                    Between (AgentSet.(diff aSet v),AgentSet.(diff aSet u))
              ) hmap
            )
          ) tmap
        ) 
      ) x in
      res

    | Or (phi1,phi2) ->
      let x = setSemAux phi1 h_ in
      let y = setSemAux phi2 h_ in
      let res = union x y in
      res

    |And (phi',phi2) -> setSemAux (Not (Or(Not phi', Not phi2))) h_

    |Exists (a,phi') ->
      let aa = AgentSet.fold(
        fun s acc -> AgentMap.add s ((fun b ->  setSemAux (setAgentFormula a b phi') h_) s) acc
      ) aSet AgentMap.empty in
      AgentMap.fold (fun _ s acc -> union s acc) aa FloatMap.empty

    |Forall (a,phi') -> setSemAux (Not(Exists(a,Not phi'))) h_

    |Diam(d1, d2, phi') -> 
      let x = setSemAux phi' h_ in
      let ress = FloatMap.merge(
        fun _ a b -> match a,b with
          | Some(tmap), Some(tGraph) -> Some (
            let (tmp,_tmp_inf,_tmp_sup) = _get_list_edges aSet tGraph d1 d2 pond_funct in
            FloatMap.map( fun hmap -> 
              let lhmap = FloatMap.bindings hmap in
              let res = ref ((merge_all_edges_sem (construct lhmap [] tmp) (construct_rest [] _tmp_inf) (construct_rest [] _tmp_sup)) |> List.to_seq |> FloatMap.of_seq )in
              rm_redundant_key res;
              rm_big_key res ss;
              !res
            ) tmap
          )
          | _, _ -> None
      ) x osig in
      ress

    |F (a, b, phi') ->
      let semx = setSemAux phi' h_ in
      let res = (*ref*) 
      (FloatMap.mapi (fun t _ -> 
        FloatMap.fold (fun t' hmap acc -> 
          if t+.a <= t' && t' <= t+.b then (
            let shift_hmap = _shift (t'-.t) hmap aSet in
            FloatMap.merge(
              fun h smap1 smap2 -> if h > h_ then None else(
                merge_shift_by_hor shift_hmap acc smap1 smap2 h
                )
            ) shift_hmap acc )
          else acc
        ) semx ( singleton_hSig 0. (Between (AgentSet.empty,aSet))) 
      ) osig) in
      (* res :=  *)
      FloatMap.mapi(fun t hmap ->
        FloatMap.mapi (fun h smap -> 
            if ( Float.add h t > b) then (
              FloatMap.mapi(fun k (Between(u,v)) -> 
                if Float.is_infinite k then (Between(u,u))
                else Between(u,v)) smap
              )
            else smap) hmap
        ) res 

        (* !res;
      rm_redundant_h_key res;
      !res *)

 (* à priori ok*)
    | Mu (_x,phi') -> 
      let quit_loop = ref false in
      x_ := empty_tSigA osig;
      while not !quit_loop do
        let _pred_x = !x_ in
        let semx = setSemAux phi' h_ in
        if FloatMap.equal (fun hx hy -> 
          FloatMap.equal ( fun sx sy ->
            FloatMap.equal ( fun (Between(ux,vx)) (Between(uy,vy)) -> 
              AgentSet.equal ux uy && AgentSet.equal vx vy) sx sy )hx hy ) !x_ semx
        then quit_loop := true
        else x_ := semx
      done;
      !x_
      
    | Var _x -> 
      !x_

       (* Ajouter couche de Floatmap*)
    |H (h_', phi') ->
    let semx = setSemAux phi' (Float.min h_ h_') in
    (*FloatMap.map( 
      fun tmap -> (
        match FloatMap.find_opt h_' tmap with
          |Some (_) -> tmap
          |None -> let(_,ss) = FloatMap.find_last (fun h -> h <= h_') tmap in
                    FloatMap.add h_' (FloatMap.map( fun (Between(u,_)) -> Between(u,u) ) ss) tmap
      )
    ) *)semx

    |SHor(s,phi') ->
      let semx = setSemAux phi' h_ in
      FloatMap.map 
      (fun hmap -> 
        FloatMap.map 
        (fun smap ->
          match FloatMap.find_last_opt(fun s' -> s' <= s) smap with
            |Some(_,amap) -> 
              let amap' = (fun (Between(u,_)) -> Between (u,u)) amap in
              smap
              |> FloatMap.filter (fun key _ -> key < s)
              |> FloatMap.add s amap'
              |> FloatMap.add infinity amap'
            |_ -> smap
          ) hmap
        ) semx

    |_ -> FloatMap.empty
    
        in
        setSemAux phi h_
