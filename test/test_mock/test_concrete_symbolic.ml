open Test.Utils
open Memory_models
module Obj = Object_mwl.M

(* Test 1: pc = true
    Concrete + Concrete *)
let () =
  Printf.printf "Test2: Concrete + Symbolic\n";

  let pc = value_bool true in
  let props = List.map key_c [ "a"; "b"; "c" ] in
  let values = List.map value_int [ 1; 2; 3 ] in

  let obj = Obj.create () in
  assert (Obj.to_list obj = []);

  let obj =
    List.fold_left2
      (fun obj prop value ->
        let obj, _ = get_obj (Obj.set obj ~field:prop ~data:value pc) in
        obj )
      obj props values
  in

  assert (Obj.has_field obj (key_c "a") pc = value_bool true);
  assert (Obj.has_field obj (key_c "b") pc = value_bool true);
  assert (Obj.has_field obj (key_c "c") pc = value_bool true);

  Printf.printf "Object state: %s\n" (Obj.to_string obj);

  let symb_prop = key_s "prop" in

  assert (
    Obj.get obj symb_prop pc
    = [ ( ite
            (eq symb_prop (List.nth props 2))
            (List.nth values 2)
            (ite
               (eq symb_prop (List.nth props 1))
               (List.nth values 1)
               (ite
                  (eq symb_prop (List.nth props 0))
                  (List.nth values 0) undef ) )
        , pc )
      ] )
