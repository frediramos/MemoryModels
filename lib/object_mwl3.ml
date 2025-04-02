open Utils.Encoding

type value = Smtml.Expr.t
type pc_value = Smtml.Expr.t

module M = struct
  type t =
    | Empty
    | Seq of t * t
    | Rec of (value * value)
    | If of (pc_value * t * t)

  let create () : t = Empty

  let set obj prop v pc =
    if pc = true_ then Seq (Rec (prop, v), obj)
    else Seq (If (pc, Rec (prop, v), Empty), obj)

  let extend_with_cond lst b = List.map (fun (v, cond) -> (v, and_ cond b)) lst

  let rec get_vals (_obj : t) (_prop : value) (_pc : pc_value) :
    (value * pc_value) list * pc_value =
    if not (is_sat [ _pc ]) then ([], true_)
    else
      match _obj with
      | Empty -> ([], true_)
      | Rec (p, v) ->
        if is_sat [ eq _prop p ] then ([ (v, eq _prop p) ], ne _prop p)
        else ([], true_)
      | Seq (o1, o2) ->
        let lst1, pc1 = get_vals o1 _prop _pc in
        let lst2, pc2 = get_vals o2 _prop (and_ _pc pc1) in
        (lst1 @ lst2, and_ pc1 pc2)
      | If (cond, o1, o2) ->
        let lst1, pi_1 = get_vals o1 _prop (and_ _pc cond) in
        let lst2, pi_2 = get_vals o2 _prop (and_ _pc (not_ cond)) in
        let lst1' = extend_with_cond lst1 cond in
        let lst2' = extend_with_cond lst2 (not_ cond) in
        (lst1' @ lst2', or_ (and_ cond pi_1) (and_ (not_ cond) pi_2))

  let rec mk_ite_v (lst : (value * pc_value) list) : value =
    match lst with
    | [] -> undef
    | [ (v, pc) ] -> ite pc v undef
    | (v, pc) :: rest -> ite pc v (mk_ite_v rest)

  let get (obj : t) (prop : value) (pc : pc_value) : value =
    let lst, _ = get_vals obj prop pc in
    let v = mk_ite_v lst in
    v

  let rec to_string (o : t) : string =
    match o with
    | Empty -> "Empty"
    | Rec (p, v) ->
      Fmt.asprintf "Rec(%s, %s)" (Smtml.Expr.to_string p)
        (Smtml.Expr.to_string v)
    | If (cond, o1, o2) ->
      Fmt.asprintf "If(%s, %s, %s)"
        (Smtml.Expr.to_string cond)
        (to_string o1) (to_string o2)
    | Seq (o1, o2) -> Fmt.asprintf "Seq(%s, %s)" (to_string o1) (to_string o2)
end
