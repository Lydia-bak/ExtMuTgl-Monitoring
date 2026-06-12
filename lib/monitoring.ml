(*
open Types


let monitoring (osig : graph signal) (phi : Tformula.t) (aSet : AgentSet.t) : unit =
  let mu_cache = ref (FloatMap.map (fun _ -> FloatMap.of_seq @@ List.to_seq [(0.,exactly AgentSet.empty);]) osig )in
  let hmax = Tformula.hSize(phi) in
  if Float.equal hmax infinity then print_endline ("Fail : hmax = infty") (* Todo : Exception using effect handlers *)
  else
    let sig_window = ref (FloatMap.filter(
      fun t _ -> t < hmax
    ) osig) in
    let t = ref 0. in

    while true do 

      let _sem = Tformula.bSem phi aSet !sig_window mu_cache in
      let () = print_endline ("yield : ") in
      sig_window := FloatMap.remove !t !sig_window;
      let (tt,_) = FloatMap.find_first(fun _ -> true) !sig_window in
      t := tt;
      sig_window := FloatMap.filter(
        fun s _ -> s < !t+.hmax && s >= !t
      ) osig 

    done;

    print_endline ("done ");

*)
      
