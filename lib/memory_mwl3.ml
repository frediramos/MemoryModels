open Utils.Encoding
open Smtml

module Make (Obj : sig
  type t
  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t

  val create : unit -> t
  val clone : t -> t
  val set : t -> value -> value -> pc_value -> (t * pc_value) list
  val get_lst : t -> value -> pc_value -> (value * pc_value) list
  val to_string : (t * pc_value) list -> string
end) =
struct
  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t
  type object_ = Obj.t
  type hrec = (string, object_) Hashtbl.t
  type t = hrec list

  let create () = [ Hashtbl.create 512 ]

  let alloc (h : t) (l : string) : value =
    let o = Obj.create () in
    let head = List.hd h in
    Hashtbl.replace head l o;
    Expr.(make @@ Val (Str l))

  let extend_heap h loc obj : t =
    let hrec = Hashtbl.create 1 in
    Hashtbl.replace hrec loc obj;
    hrec :: h

  let rec find h loc =
    match h with
    | [] -> failwith "Could not find Object"
    | hrec :: h' -> (
      match Hashtbl.find_opt hrec loc with
      | None -> find h' loc
      | Some obj -> Obj.clone obj )

  let replace h prop obj =
    match h with
    | [] -> failwith "Invalid Heap: Empty"
    | hrec :: _ -> Hashtbl.replace hrec prop obj

  let possible_locs (v : value) (pc : pc_value) : (string * pc_value) list =
    let rec possible_locs' v pc =
      if not (is_sat [ pc ]) then []
      else
        match Expr.view v with
        | Val (Str loc) -> [ (loc, true_) ]
        | Triop (_, _, c, e1, e2) ->
          if pc => c then possible_locs' e1 pc
          else if pc => not_ c then possible_locs' e2 pc
          else
            let r1 = possible_locs' e1 (and_ pc c) in
            let r2 = possible_locs' e2 (and_ pc (not_ c)) in
            r1 @ r2
        | _ -> failwith "Error: Invalid Mem Location"
    in
    possible_locs' v pc

  let rec mk_ite_v (lst : (value * pc_value) list) : value =
    match lst with
    | [] -> undef
    | [ (v, pc) ] -> ite pc v undef
    | (v, pc) :: rest -> ite pc v (mk_ite_v rest)

  let rec get_lst_aux h loc prop pc =
    match h with
    | [] -> []
    | hrec :: h' -> (
      match Hashtbl.find_opt hrec loc with
      | None -> get_lst h' (str loc) prop pc
      | Some obj -> Obj.get_lst obj prop pc )

  and get_lst h loc prop pc =
    let locs = possible_locs loc pc in
    List.concat
      (List.map
         (fun (loc', pc') ->
           let rets = get_lst_aux h loc' prop (and_ pc pc') in
           List.map (fun (v, pc'') -> (v, and_ pc pc'')) rets )
         locs )

  let get (h : t) (l : value) (p : value) (pc : pc_value) : value =
    let rets = get_lst h l p pc in
    mk_ite_v rets

  let set h loc prop v pc =
    let locs = possible_locs loc pc in
    match locs with
    | [] -> failwith "Could not find location"
    | [ (l, pc') ] ->
      assert (pc' = true_);
      let obj = find h l in
      let rets = Obj.set obj prop v pc' in
      List.map (fun (obj, pc') -> (extend_heap h l obj, pc')) rets
    | _ ->
      List.concat
        (List.map
           (fun (l, pc') ->
             let obj = find h l in
             (* É preciso copiar sempre *)
             let rets = Obj.set obj prop v (and_ pc pc') in
             List.map
               (fun (obj, pc'') -> (extend_heap h l obj, and_ pc' pc''))
               rets )
           locs )

  let to_string_heap (heap : hrec) (pc : pc_value) =
    let first = ref true in
    let s =
      Hashtbl.fold
        (fun l o acc ->
          let obj_string = l ^ " -> " ^ "{" ^ Obj.to_string [ (o, pc) ] ^ "}" in
          if !first then (
            first := false;
            acc ^ obj_string )
          else acc ^ "; " ^ obj_string )
        heap "[[ "
    in
    s ^ " ]]"

    let to_string_heap_list (hlist: t) (pc: pc_value) : string =
    let first = ref true in
    let hlist = List.rev hlist in
    let s =
      List.fold_left
        (fun acc h ->
          let heap_string = to_string_heap h pc in
          if !first then (
            first := false;
            acc ^ heap_string )
          else acc ^ ",\n" ^ heap_string )
        "" hlist
    in
    s

  let to_string (pairs : (t * pc_value) list) : string =
    let first = ref true in
    let s =
      List.fold_left
        (fun acc (h, pc) ->
          let heap_string = to_string_heap_list h pc in
          if !first then (
            first := false;
            acc ^ heap_string )
          else acc ^ "\n\n" ^ heap_string )
        "" pairs
    in
    s
end

module M = Make (Object_mwl6.M)
include M
