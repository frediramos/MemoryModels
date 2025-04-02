open Utils.Encoding
open Smtml

module Make (Obj : sig
  type t
  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t

  val create : unit -> t
  val clone : t -> t
  val set : t -> value -> value -> pc_value -> (t * pc_value) list
  val get : t -> value -> pc_value -> value
  val to_string : (t * pc_value) list -> string
end) =
struct
  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t
  type object_ = Obj.t
  type t = (string, object_) Hashtbl.t

  let create () = Hashtbl.create 512

  let alloc (h : t) (l : string) : value =
    let o = Obj.create () in
    Hashtbl.replace h l o;
    Expr.(make @@ Val (Str l))

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

  let get (h : t) (l : value) (p : value) pc =
    let locs = possible_locs l pc in
    let objs = List.map (fun (l, pc') -> (Hashtbl.find h l, pc')) locs in
    let rets =
      List.map (fun (o, pc') -> (Obj.get o p (and_ pc pc'), pc')) objs
    in
    mk_ite_v rets

  let set h l p v pc =
    let locs = possible_locs l pc in
    let objs = List.map (fun (l, pc') -> (l, Hashtbl.find h l, pc')) locs in
    let rets =
      List.concat
        (List.map
           (fun (l, o, pc') ->
             let o = Obj.clone o in
             let rets' = Obj.set o p v (and_ pc pc') in
             List.map (fun (o, pc) -> (l, o, and_ pc pc')) rets' )
           objs )
    in

    match rets with
    | [ (l, o, pc') ] ->
      assert (pc' = true_);
      Hashtbl.replace h l o;
      [ (h, pc') ]
    | _ ->
      (* branching *)
      List.map
        (fun (l, o, pc') ->
          let h' = Hashtbl.copy h in
          Hashtbl.replace h' l o;
          (h', pc') )
        rets

  let to_string_heap (heap : t) (pc : pc_value) =
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

  let to_string (pairs : (t * pc_value) list) : string =
    let first = ref true in
    let s =
      List.fold_left
        (fun acc (h, pc) ->
          let heap_string = to_string_heap h pc in
          if !first then (
            first := false;
            acc ^ heap_string )
          else acc ^ ",\n" ^ heap_string )
        "" pairs
    in
    s
end

module M = Make (Object_mwl6.M)
include M
