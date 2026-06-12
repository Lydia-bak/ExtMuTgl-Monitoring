type fpv = string
type agent = string 
type coord = float * float

type 'a between = Between of 'a * 'a
type couple = Couple of float * float

let exactly x = Between (x,x)

module AgentSet = Set.Make(String)
module AgentMap = Map.Make(String)
type graph = (float*float) AgentMap.t

module StringPair = struct
  type t = string * string
  let compare (a1, b1) (a2, b2) =
    let c = String.compare a1 a2 in
    if c <> 0 then c else String.compare b1 b2
end

module CoupleSet = Set.Make(StringPair)

let distance (a,b) (c,d) = Float.sqrt((a-.c)*.(a-.c)+.(b-.d)*.(b-.d))

type uncertainSet = AgentSet.t between
module FloatMap = Map.Make(Float)

let b_is_equal (Between(u1,v1) : uncertainSet) (Between(u2,v2) : uncertainSet) :bool = (AgentSet.equal u1 u2) && (AgentSet.equal v1 v2)
type 'a signal = 'a FloatMap.t 

module Bool3 = struct
  type t =
    |Top
    |Bot
    |Undet

  let equal a b = match a,b with 
    |Top, Top |Bot, Bot |Undet, Undet-> true
    |_,_ -> false

  let to_string = function
    |Top -> "Top"
    |Bot -> "Bot"
    |Undet -> "Undet"

  let neg = function
    |Top -> Bot
    |Bot -> Top
    |Undet -> Undet

  let boolOr _ a b = match (a,b) with 
    | Top, _ | _, Top -> Some(Top) 
    | Undet, _ | _, Undet -> Some(Undet)
    |_,_ -> Some(Bot) 
end

module FloatCouple = struct
  type t = float * float
  let compare = compare
end

module FFMap = Map.Make(FloatCouple)

(*Initialization functions for signals*)
let empty_sSigA = FloatMap.add infinity (exactly AgentSet.empty)(
  FloatMap.add 0. (exactly AgentSet.empty) FloatMap.empty )
let empty_sSigA_undet aSet = FloatMap.add 0. (Between (AgentSet.empty,aSet)) FloatMap.empty
let empty_hSigA = FloatMap.add 0. empty_sSigA FloatMap.empty
let empty_tSigA oSig = FloatMap.map (fun _ -> empty_hSigA) oSig
let empty_tSigA2 = FloatMap.add 0. empty_hSigA FloatMap.empty
let singleton_sSig s uSet = FloatMap.add s (uSet) FloatMap.empty
let singleton_hSig h uSet = FloatMap.add h (singleton_sSig 0. uSet) FloatMap.empty


(*Union and merge functions*)

let b_union (Between(u1,v1)) (Between(u2,v2)) = Between (AgentSet.union u1 u2, AgentSet.union v1 v2)
let b_union_u (Between(u1,_)) (Between(u2,_)) = Between (AgentSet.union u1 u2, AgentSet.union u1 u2)

(*let merge_sig f (sig1 : 'a FloatMap.t) (sig2 : 'a FloatMap.t) =
    FloatMap.merge (fun _ x y -> match (x,y) with
      |(Some(hmap1),Some(hmap2)) -> Some (f hmap1 hmap2)
      |(Some(hmap),None) | (None, Some(hmap)) -> Some(hmap)
      |(_,_) -> None
    ) sig1 sig2*)
let find_last_or_sem s sem1 sem2 = match FloatMap.find_last_opt(fun s' -> s'<= s ) sem1 with 
    |Some((_,x)) -> x;
    |None -> sem2
    
let merge_sig merge_function sig1 sig2 =
    FloatMap.merge(fun t map1 map2 -> match map1,map2 with
      | (Some(m1), Some(m2)) -> Some(merge_function m1 m2)
      | (None,Some(m2)) -> 
        let m1 = find_last_or_sem t sig1 m2 in
        Some(merge_function m1 m2)
      | (Some(m1),None) -> 
        (*let (_,m2) = FloatMap.find_last(fun t' -> t'<= t ) sig2 in*)
        let m2 = find_last_or_sem t sig2 m1 in
        Some(merge_function m1 m2)
      |(None,None) -> None
    ) sig1 sig2

(*let merge_sig_end f f' (sig1 : 'a FloatMap.t) (sig2 : 'a FloatMap.t) =
    FloatMap.merge (fun s x y -> match (x,y) with
      |(Some(hmap1),Some(hmap2)) -> 
        if Float.is_infinite s then Some (f' hmap1 hmap2)
        else Some (f hmap1 hmap2)
      |(Some(hmap),None) | (None, Some(hmap)) -> Some(hmap)
      |(_,_) -> None
    ) sig1 sig2*)


let merge_sig_end f' f sig1 sig2 =
    FloatMap.merge(fun s map1 map2 -> match map1,map2 with
      | (Some(m1), Some(m2)) -> 
        if Float.is_infinite s then Some (f' m1 m2)
        else Some(f m1 m2)
      | (None,Some(m2)) -> 
        let m1 = find_last_or_sem s sig1 m2 in
        if Float.is_infinite s then Some (f' m1 m2)
        else Some(f m1 m2)
      | (Some(m1),None) -> 
        (*let (_,m2) = FloatMap.find_last(fun t' -> t'<= t ) sig2 in*)
        let m2 = find_last_or_sem s sig2 m1 in
        if Float.is_infinite s then Some (f' m1 m2)
        else Some(f m1 m2)
      |(None,None) -> None
    ) sig1 sig2


let merge_smap smap1 smap2 = merge_sig b_union smap1 smap2

let is_subset (Between (u1,v1)) (Between (u2,v2)) =
  if (AgentSet.(subset u1 u2) && AgentSet.(subset v1 v2)) then Bool3.Top
  else if AgentSet.(subset v1 v2) then Bool3.Undet
  else Bool3.Bot




