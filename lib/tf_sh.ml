(** Module Tformula
  * specifies the form of the truth formulas along with their semantic.
*)
open Types

  type t = 
    | Top
    | Not of t
    | And of t * t
    | Or of t * t
    | Implies of t * t 
    | Exists of agent * t
    | Forall of agent * t
    | Leq of Af_sh.t * Af_sh.t
    | Eq of Af_sh.t * Af_sh.t
    | F of float * float * t


  (** Horizon size of a formula *)
  let rec hSize (phi : t) : float = match phi with
    | Top -> 0.
    | Not phi1 -> hSize phi1
    | Exists (_, phi1) -> hSize phi1
    | Forall (_, phi1) -> hSize phi1
    | F (_, t, phi1) -> hSize phi1 +. t
    | Leq (phi1, phi2) | Eq (phi1, phi2) -> max(Af_sh.hSize phi1) (Af_sh.hSize phi2)
    | And (phi1, phi2) | Or (phi1, phi2) | Implies (phi1, phi2) -> max(hSize phi1) (hSize phi2)


  (** Substitute the agent variable a with b in a formula
    * Used for Exists and Forall operators 
    *)
  let rec setAgentFormula (a) (b) (phi)  : t = match phi with
    | Top -> Top
    | Not phi1 -> Not (setAgentFormula a b phi1)
    | Exists (c, phi1) -> Exists(c, setAgentFormula a b phi1)
    | Forall (c, phi1) -> Forall(c, setAgentFormula a b phi1)
    | F (s, t, phi1) -> F(s,t,setAgentFormula a b phi1)
    | And (phi1, phi2) -> And (setAgentFormula a b phi1,setAgentFormula a b phi2)
    | Or (phi1, phi2) -> Or (setAgentFormula a b phi1, setAgentFormula a b phi2)
    | Implies (phi1, phi2) -> Implies (setAgentFormula a b phi1, setAgentFormula a b phi2)
    | Leq (phi1, phi2) -> Leq (Af_sh.setAgentFormula a b phi1, Af_sh.setAgentFormula a b phi2)
    | Eq (phi1, phi2) -> Eq (Af_sh.setAgentFormula a b phi1, Af_sh.setAgentFormula a b phi2)



  let union (x: Bool3.t signal signal) (y: Bool3.t signal signal) = 
    FloatMap.union 
        (fun _ sigx sigy -> Some (FloatMap.union Bool3.boolOr sigx sigy)) 
        x y

    let delay t0 t tt a b semX =
        let fMap = FloatMap.empty in 
        let fMap = FloatMap.add t0 (FloatMap.of_seq @@ List.to_seq [(0.,Bool3.Bot);]) fMap in
        let fMap = FloatMap.add (tt-.a) (FloatMap.of_seq @@ List.to_seq [(0.,Bool3.Bot);]) fMap in
        let fMap = FloatMap.add (t-.b) (FloatMap.of_seq @@ List.to_seq [(t,Bool3.Bot);]) fMap in
        let (_,hmap) = FloatMap.find_last (fun t' -> t' <= (max (t-.b) 0.)) semX in
        FloatMap.add (t-.b) (FloatMap.fold (
          fun h _ acc -> 
            if h > b then let (_,res) = (FloatMap.find_last (fun h' -> h' <= h+.b) hmap) in FloatMap.add (h+.b) res acc
            else  FloatMap.add h (Bool3.Bot) acc
            
        ) (hmap) (FloatMap.find (max (t-.b) 0.) fMap) ) fMap

  (** Todo = rewrite better
  *)

  let rec bSem (phi : t) (aSet : AgentSet.t) (osig : graph signal) (x_: uncertainSet signal signal signal ref) (pond_funct:(agent * coord -> agent * coord -> float)) : Bool3.t signal signal = 
    match phi with
    |Top -> FloatMap.map (fun _ -> FloatMap.of_seq @@ List.to_seq [(0.,Bool3.Top);]) osig

    |Not phi1 -> 
      
      let x = bSem phi1 aSet osig x_ pond_funct in
      FloatMap.map( 
        fun tmap -> (
          FloatMap.map
            (function 
                |Bool3.Top -> Bool3.Bot
                |Bool3.Bot -> Bool3.Top
                |Bool3.Undet -> Bool3.Undet
            )(tmap))
      ) x
      
    | Or (phi1,phi2) ->
        let x = bSem phi1 aSet osig x_ pond_funct in
        let y = bSem phi2 aSet osig x_ pond_funct in
        FloatMap.(union (fun _ sigx sigy ->
          Some (FloatMap.(union (fun _ (a) (b) -> match (a,b) with
            |(Bool3.Top,_)|(_,Bool3.Top) -> Some Bool3.Top
            |(Bool3.Undet,_)|(_,Bool3.Undet) -> Some Bool3.Undet
            |_ -> Some Bool3.Bot
            
          ))sigx sigy)) x y
        )

    |And (phi1,phi2) -> bSem (Not (Or(Not phi1, Not phi2))) aSet osig x_ pond_funct

    |Implies(phi1,phi2) -> bSem(Or(Not phi1, phi2)) aSet osig x_ pond_funct

    |Exists (a,phi') ->
      let aa = AgentSet.fold(
          fun s acc -> AgentMap.add s ((fun b ->  bSem (setAgentFormula a b phi') aSet osig x_ pond_funct) s) acc 
        ) aSet AgentMap.empty in

      AgentMap.fold (fun _ s acc -> union s acc) aa FloatMap.empty

    |Forall (a,phi') -> bSem (Not(Exists(a,Not phi'))) aSet osig x_ pond_funct

    |Leq (phi1,phi2) -> 
      let x = Af_sh.setSem phi1 aSet osig x_ Float.infinity pond_funct in
      let y = Af_sh.setSem phi2 aSet osig x_ Float.infinity pond_funct in
      
      FloatMap.merge (
        fun _ aa bb -> match (aa, bb) with 
          |Some(a),Some(b) -> Some (FloatMap.mapi (
            fun h bUs -> 
  
              let (_,aUs) = FloatMap.find_last(fun h' -> h' <= h) a in
              let (_,aUsLast) = FloatMap.find_last(fun _ -> true) aUs in
              let (_,bUsLast) = FloatMap.find_last(fun _ -> true) bUs in
              is_subset aUsLast bUsLast
          )b)
          |(_,_) -> 
            Some (FloatMap.empty)
      ) x y 
      
    |Eq (phi1,phi2) -> 
      bSem(And (Leq (phi1,phi2), Leq (phi2,phi1))) aSet osig x_ pond_funct
      
    |F (a, b, phi') -> 
      let x = bSem phi' aSet osig x_ pond_funct in
      let _,coupleMap = FloatMap.fold (
        fun t _ (t_prec_opt,acc) -> match t_prec_opt with 
          |None -> (Some t, acc)
          |Some tp -> Some t, FFMap.add (tp, t) (delay 0. tp t a b x) acc
        ) x (None, FFMap.empty) in
      
      FFMap.fold (fun _ s acc -> union s acc) coupleMap (FloatMap.empty: Bool3.t signal signal)
