open Test.Utils
open Memory_models
module Obj = Object_mwl.M

(* Test 1: pc = true
    Concrete + Concrete *)
let () =
  Printf.printf "Test1: Concrete + Concrete\n";

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

  Printf.printf "Object state: %s\n" (Obj.to_string obj);

  assert (Obj.get obj (List.nth props 0) pc = [ (List.nth values 0, pc) ]);
  assert (Obj.get obj (List.nth props 1) pc = [ (List.nth values 1, pc) ]);
  assert (Obj.get obj (List.nth props 2) pc = [ (List.nth values 2, pc) ])
