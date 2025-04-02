open Test.Utils
open Memory_models
module Obj = Object_mwl.M

(* Test 1: pc = true
    Concrete + Concrete *)
let () =
  Printf.printf "Test4: Symbolic + Symbolic\n";

  let pc = value_bool true in
  let props = List.map key_s [ "a"; "b"; "c" ] in
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

  assert (Obj.has_field obj (key_s "a") pc = value_bool true);
  assert (Obj.has_field obj (key_s "b") pc = value_bool true);
  assert (Obj.has_field obj (key_s "c") pc = value_bool true);

  Printf.printf "Object state: %s\n" (Obj.to_string obj);

  let concr_prop = key_s "prop" in

  assert (
    Obj.get obj concr_prop pc
    = [ ( ite
            (eq concr_prop (List.nth props 2))
            (List.nth values 2)
            (ite
               (eq concr_prop (List.nth props 1))
               (List.nth values 1)
               (ite
                  (eq concr_prop (List.nth props 0))
                  (List.nth values 0) undef ) )
        , pc )
      ] )
