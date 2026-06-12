(** Module Aformula
  * specifies the form of the agent formulas along with their semantic.
*)
(*
open Types



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



let union (x: uncertainSet signal signal) (y: uncertainSet signal signal) = 
  FloatMap.(union (fun _ sigx sigy ->
          Some (FloatMap.(union (fun _ (Between (ux, vx)) (Between (uy,vy)) ->
            Some(Between (AgentSet.(union ux uy) , AgentSet.(union vx vy)))
          ))sigx sigy)) x y)


let shift s hmap aSet = match s with 
    |0. -> FloatMap.update 0. (function  
        |(Some(Between(u,_))) -> Some(Between(u,aSet))
        |None -> None
        )  hmap

    |_ -> let res = FloatMap.of_seq @@ List.to_seq [(0.,Between(AgentSet.empty, aSet));] in
      FloatMap.fold(fun h uSet acc ->
        FloatMap.add (h+.s) uSet acc
    ) hmap res
        
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




  let rec setSem (phi : t) (aSet : AgentSet.t) (osig : graph signal) (x_: uncertainSet signal signal ref) (h_:float) : uncertainSet signal signal = 
    match phi with

    |Top -> FloatMap.map (fun _ -> FloatMap.of_seq @@ List.to_seq [(0.,exactly aSet);]) osig
    
    
    |Ag a -> FloatMap.map (fun _ -> FloatMap.of_seq @@ List.to_seq [(0.,exactly (AgentSet.singleton a));]) osig
    
    
    |Not phi1 -> 
      let x = setSem phi1 aSet osig x_ h_ in
      FloatMap.map( 
        fun tmap -> (
          FloatMap.map
            (fun (Between (u, v)) -> 
                    Between (AgentSet.(diff aSet v),AgentSet.(diff aSet u)))
            )(tmap)
      ) x


    | Or (phi1,phi2) ->
      let x = setSem phi1 aSet osig x_ h_ in
      let y = setSem phi2 aSet osig x_ h_ in
      FloatMap.(union (fun _ sigx sigy ->
        Some (FloatMap.(union (fun _ (Between (ux, vx)) (Between (uy,vy)) ->
          Some(Between (AgentSet.(union ux uy) , AgentSet.(union vx vy)))
        ))sigx sigy)) x y
      )


    |And (phi',phi2) -> setSem (Not (Or(Not phi', Not phi2))) aSet osig x_ h_


    |Exists (a,phi') ->
      let aa = AgentSet.fold(
        fun s acc -> AgentMap.add s ((fun b ->  setSem (setAgentFormula a b phi') aSet osig x_ h_) s) acc
      ) aSet AgentMap.empty in
      AgentMap.fold (fun _ s acc -> union s acc) aa FloatMap.empty


    |Forall (a,phi') -> setSem (Not(Exists(a,Not phi'))) aSet osig x_ h_


    |Diam(d1, d2, phi') -> 
      let x = setSem phi' aSet osig x_ h_ in
      FloatMap.merge(
        fun _ a b -> match a,b with
        | Some(tmap), Some(tGraph) -> Some (
          FloatMap.map (
            fun (Between (u,v)) ->  (neighbours_from_aset aSet tGraph d1 d2 u v )
          ) tmap)
        | _, _ -> None

      ) x osig


    |F (a, b, phi') ->
      let semx = setSem phi' aSet osig x_ h_ in
      FloatMap.mapi (fun t _-> 
        FloatMap.fold (fun t' hmap acc -> 
          if t+.a <= t' && t' <= t+.b then 
            let shift_hmap = shift (t'-.t) hmap aSet in
            FloatMap.merge(
              fun h uset1 uset2 -> if h > h_ then None else
                merge_shift_by_hor shift_hmap acc uset1 uset2 h b t
            ) shift_hmap acc
          else acc
        ) semx (FloatMap.of_seq @@ List.to_seq [(0.,Between (AgentSet.empty,aSet));])
      ) osig


    | Mu (_x,phi') -> 
      let quit_loop = ref false in
      while not !quit_loop do
        let pred_x = !x_ in
        let semx = setSem phi' aSet osig x_  h_ in
        let () = print_endline ("new sem :") in
        let () = print_endline (string_of_string_set_between_signal semx) in
        if FloatMap.equal (fun hx hy -> FloatMap.equal (fun (Between(ux,vx)) (Between(uy,vy)) -> AgentSet.equal ux uy && AgentSet.equal vx vy) hx hy ) pred_x semx then quit_loop := true
        else x_ := semx
      done;
      let () = print_endline ("Fixed point found") in
      !x_
      
      
    | Var _x -> 
      let () =  print_endline ("Current value of fixed point : ") in
      let () = print_endline (string_of_string_set_between_signal !x_) in
      !x_

      
     |H (h_', phi') ->
      let semx = setSem phi' aSet osig x_ h_ in
      FloatMap.map( 
        fun tmap -> (
          FloatMap.mapi
            (fun h uSet -> if h >= h_' then 
              let (_,Between(u,_)) = FloatMap.find_last(fun h'' -> h''<= h_' ) tmap in Between(u,u)
          else uSet) tmap
      )) semx


    |_ -> FloatMap.empty



  let is_subset (Between (u1,v1)) (Between (u2,v2)) =
    if (AgentSet.(subset u1 u2) && AgentSet.(subset v1 v2)) then Bool3.Top
    else if AgentSet.(subset v1 v2) then Bool3.Undet
    else Bool3.Bot

*)