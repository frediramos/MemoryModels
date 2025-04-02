open Utils.Encoding

type value = Smtml.Expr.t
type pc_value = Smtml.Expr.t

module M = struct
  type t = t' list
  and t' = Rec of (value * value) | If of (pc_value * t * t)

  let create () : t = []

  let set obj prop v pc =
    if pc = true_ then Rec (prop, v) :: obj
    else If (pc, [ Rec (prop, v) ], []) :: obj

  let extend_with_cond lst b = List.map (fun (v, cond) -> (v, and_ cond b)) lst

  let rec get_vals' (obj : t') (prop : value) (pc : pc_value) :
    (value * pc_value) list * pc_value =
    match obj with
    | Rec (prop', v) ->
      if is_sat [ eq prop' prop ] then ([ (v, eq prop prop') ], ne prop prop')
      else ([], true_)
    | If (cond, o1, o2) ->
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

  let get (obj : t) (prop : value) (pc : pc_value) : value =
    let lst, _ = get_vals obj prop pc in
    let v = mk_ite_v lst in
    v

  let collapse (o1 : t') (o2 : t') : t' option =
    match o1 with
    | Rec (_, _) -> None
    | If (cond1, v1, _) -> (
      match o2 with
      | Rec (_, _) -> None
      | If (cond2, v2, _) ->
        let cond1 = Smtml.Expr.simplify cond1 in
        let neg_cond2 = Smtml.Expr.simplify (not_ cond2) in
        if Smtml.Expr.equal cond1 neg_cond2 then Some (If (cond1, v1, v2))
        else None )

  let rec simplify_obj (o : t) : t =
    match o with
    | [] -> []
    | [ r ] -> [ r ]
    | r1 :: r2 :: o' -> (
      let r = collapse r1 r2 in
      match r with
      | None -> r1 :: simplify_obj (r2 :: o')
      | Some r' -> r' :: simplify_obj o' )

  let rec to_string (o : t) =
    match o with
    | [] -> "[]"
    | [ x ] -> "[" ^ print_t' x ^ "]"
    | x :: xs -> "[" ^ print_t' x ^ ", " ^ to_string xs ^ "]"

  and print_t' (o : t') =
    match o with
    | Rec (p, v) ->
      "Rec(" ^ Smtml.Expr.to_string p ^ ", " ^ Smtml.Expr.to_string v ^ ")"
    | If (cond, o1, o2) ->
      "If(" ^ Smtml.Expr.to_string cond ^ ", " ^ to_string o1 ^ ", "
      ^ to_string o2 ^ ")"
end
