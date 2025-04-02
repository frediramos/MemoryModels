open Utils.Encoding
open Smtml

module M = struct
  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t

  type t = t' list

  and t' =
    | Srec of (value * value)
    | Crec of (string, value) Hashtbl.t
    | If of (pc_value * t * t)

  let create () : t = []

  let rec clone' (x : t') : t' =
    match x with
    | Srec (v1, v2) -> Srec (v1, v2)
    | Crec ht -> 
        let new_ht = Hashtbl.create (Hashtbl.length ht) in
        Hashtbl.iter (fun key value -> Hashtbl.add new_ht key value) ht;
        Crec new_ht
    | If (pc, t1, t2) -> If (pc, clone t1, clone t2)
  
  and clone (lst : t) : t =
    List.map clone' lst

  let set_aux (obj' : t') prop v pc : t' option =
    if pc = true_ then
      match (Expr.view prop, obj') with
      | Val (Str field), Crec tbl ->
        Hashtbl.replace tbl field v;
        None
      | Val (Str field), _ ->
        let tbl = Hashtbl.create 0 in
        Hashtbl.replace tbl field v;
        Some (Crec tbl)
      | _ -> Some (Srec (prop, v))
    else Some (If (pc, [ Srec (prop, v) ], []))

  let set (obj : t) prop v pc =
    match (obj, Expr.view prop) with
    | r :: _, _ -> (
      match set_aux r prop v pc with None -> [obj, true_] | Some r' -> [r' :: obj, true_] )
    | [], Val (Str field) ->
      let tbl = Hashtbl.create 0 in
      Hashtbl.replace tbl field v;
      [[ Crec tbl ], true_]
    | [], _ -> [[ Srec (prop, v) ], true_]

  let extend_with_cond lst b = List.map (fun (v, cond) -> (v, and_ cond b)) lst

  let rec get_vals' (obj : t') (prop : value) (pc : pc_value) :
    (value * pc_value) list * pc_value =
    match (obj, Expr.view prop) with
    | Crec tbl, Val (Str field) -> (
      match Hashtbl.find_opt tbl field with
      | Some v -> ([ (v, true_) ], false_)
      | None -> ([], true_) )
    | Crec tbl, _ ->
      let rets, not_found =
        Hashtbl.fold
          (fun prop' v (acc, not_found) ->
            let prop' = str prop' in
            if is_sat [ eq prop' prop ] then
              ((v, eq prop prop') :: acc, and_ not_found (ne prop prop'))
            else (acc, not_found) )
          tbl ([], true_)
      in

      (rets, not_found)
    | Srec (prop', v), _ ->
      if is_sat [ eq prop' prop ] then ([ (v, eq prop prop') ], ne prop prop')
      else ([], true_)
    | If (cond, o1, o2), _ ->
      let lst1, pi_1 = get_vals o1 prop (and_ pc cond) in
      let lst2, pi_2 = get_vals o2 prop (and_ pc (not_ cond)) in
      let lst1' = extend_with_cond lst1 cond in
      let lst2' = extend_with_cond lst2 (not_ cond) in
      (lst1' @ lst2', or_ (and_ cond pi_1) (and_ (not_ cond) pi_2))

  and get_vals (obj : t) (prop : value) (pc : pc_value) :
    (value * pc_value) list * pc_value =
    if not (is_sat [ pc ]) then ([], true_)
    else
      match obj with
      | [] -> ([], true_)
      | r :: obj' ->
        let lst1, pi1 = get_vals' r prop pc in
        let lst2, pi2 = get_vals obj' prop (and_ pc pi1) in
        (lst1 @ lst2, and_ pi1 pi2)

  let rec mk_ite_v (lst : (value * pc_value) list) : value =
    match lst with
    | [] -> undef
    | [ (v, pc) ] -> ite pc v undef
    | (v, pc) :: rest -> ite pc v (mk_ite_v rest)

  let get_lst (obj : t) (prop : value) (pc : pc_value) : (value * pc_value) list = 
    let lst, _ = get_vals obj prop pc in
    lst
  
  let get (obj : t) (prop : value) (pc : pc_value) : value =
    let lst = get_lst obj prop pc in
    mk_ite_v lst

  let collapse (o1 : t') (o2 : t') : t' option =
    match (o1, o2) with
    | If (cond1, v1, _), If (cond2, v2, _) ->
      let cond1 = Smtml.Expr.simplify cond1 in
      let neg_cond2 = Smtml.Expr.simplify (not_ cond2) in
      if Smtml.Expr.equal cond1 neg_cond2 then Some (If (cond1, v1, v2))
      else None
    | _ -> None

  let rec simplify_obj (o : t) : t =
    match o with
    | [] -> []
    | [ r ] -> [ r ]
    | r1 :: r2 :: o' -> (
      let r = collapse r1 r2 in
      match r with
      | None -> r1 :: simplify_obj (r2 :: o')
      | Some r' -> r' :: simplify_obj o' )

  let to_string_Crec (tbl : (string, value) Hashtbl.t) : string =
    let first = ref true in
    let s =
      Hashtbl.fold
        (fun k v acc ->
          if !first then (
            first := false;
            acc ^ k ^ " : " ^ Smtml.Expr.to_string v )
          else acc ^ ", " ^ k ^ " : " ^ Smtml.Expr.to_string v )
        tbl "{"
    in
    s ^ "}"

  let rec to_string (pair : (t * pc_value) list) : string =
    match pair with
    | [(o, _)] ->
    let first = ref true in
    let s =
      List.fold_left
        (fun acc r ->
          if !first then (
            first := false;
            acc ^ print_t' r )
          else acc ^ ", " ^ print_t' r )
        "[" o
    in
    s ^ "]"
    | _ -> failwith "Invalid Object"

  and print_t' (o : t') =
    match o with
    | Srec (p, v) -> Smtml.Expr.to_string p ^ " -> " ^ Smtml.Expr.to_string v
    | Crec tbl -> to_string_Crec tbl
    | If (cond, o1, o2) ->
      "If(" ^ Smtml.Expr.to_string cond ^ ", " ^ to_string [o1, true_] ^ ", "
      ^ to_string [o2, true_] ^ ")"
end
