(*


[ #x -> 1, #y -> 2 ]


set obj #w 3 true =

  [ #x -> 3, #y -> 2], #w = #x
  [ #x -> 1, #y -> 3], #w = #y
  [ #x -> 1, #y -> 2, #w -> 3], #w != x /\ #w != y


set obj #w 3 (#w = #x \/ #w = #y)
 [ #x -> 3, #y -> 2], #w = #x
  [ #x -> 1, #y -> 3], #w = #y



  [ #x -> 1, #y -> 2 ]


get obj #w true - (1, #w = #x), (2, #w = #y) *)

open Utils.Encoding


module M = struct
  
  type value = Smtml.Expr.t
  type pc_value = Smtml.Expr.t
  
  type t = (value, value) Hashtbl.t

  let create () : t = Hashtbl.create 10

  let clone (o: t) : t = 
    Hashtbl.copy o

  let rec mk_ite_v (lst : (value * pc_value) list) : value =
    match lst with
    | [] -> undef
    | [ (v, pc) ] -> ite pc v undef
    | (v, pc) :: rest -> ite pc v (mk_ite_v rest)

  let get_vals (obj : t) (prop : value) (pc : pc_value) :
    (value * pc_value) list =
    Hashtbl.fold
      (fun p v acc ->
        if is_sat [ pc; eq p prop ] then (v, eq p prop) :: acc else acc )
      obj []


  let get_lst (obj : t) (prop : value) (pc : pc_value) : (value * pc_value) list =
    get_vals obj prop pc
  
  let get (obj : t) (prop : value) (pc : pc_value) : value =
    let lst = get_lst obj prop pc in
    mk_ite_v lst

  let all_different (prop : value) (props : value list) : pc_value =
    List.fold_left (fun acc p -> and_ (ne prop p) acc) true_ props

  let set (obj : t) (prop : value) (v : value) (pc : pc_value) :
    (t * pc_value) list =
    (* Obter as propriedades simbolicas que podem ser iguais a prop *)
    let props =
      Hashtbl.fold
        (fun p _ acc -> if is_sat [ pc; eq p prop ] then p :: acc else acc)
        obj []
    in

    (* prop é diferente de todas as propriedades em obj *)
    if List.length props = 0 then (
      Hashtbl.replace obj prop v;
      [ (obj, true_) ] (* prop é necessariamente igual a p *) )
    else if not (is_sat [ pc; ne (List.nth props 0) prop ]) then (
      let p = List.nth props 0 in
      Hashtbl.replace obj p v;
      [ (obj, true_) ] (* prop é igual a uma ou mais propriedades de obj *) )
    else
      let rets =
        List.map
          (fun p ->
            let obj' = Hashtbl.copy obj in
            Hashtbl.replace obj' p v;
            (obj', eq prop p) )
          props
      in

      let diff = all_different prop props in
      if is_sat [ pc; diff ] then (
        Hashtbl.replace obj prop v;
        (obj, diff) :: rets )
      else rets

  let to_string_obj (obj : t) =
    let buf = Buffer.create 100 in
    let first = ref true in
    Buffer.add_string buf "[";

    let to_string_prop (p : value) (v : value) =
      if !first then first := false else Buffer.add_string buf ", ";

      Buffer.add_string buf
        (Smtml.Expr.to_string p ^ " -> " ^ Smtml.Expr.to_string v)
    in

    Hashtbl.iter to_string_prop obj;

    Buffer.add_string buf "]";
    Buffer.contents buf

  let to_string (pairs : (t * pc_value) list) =
    let buf = Buffer.create 100 in

    List.iter
      (fun (obj, pc) ->
        Buffer.add_string buf
          (to_string_obj obj ^ ", " ^ Smtml.Expr.to_string pc) )
      pairs;

    Buffer.contents buf
end
