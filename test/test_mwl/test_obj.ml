open Test.Utils
open Memory_models
module Obj = Object_mwl6.M

let () =
  let pc = value_bool true in

  let obj = Obj.create () in

  let symb_prop_z = key_s "#z" in
  let symb_prop_y = key_s "#y" in

  let symb_prop_a = key_c "a" in
  let symb_prop_b = key_c "b" in
  let symb_prop_c = key_c "c" in

  let obj,_ = get_obj (Obj.set obj symb_prop_z (value_int 1) pc) in
  let obj,_ = get_obj (Obj.set obj symb_prop_a (value_int 3) pc) in
  let obj,_ = get_obj (Obj.set obj symb_prop_y (value_int 2) pc) in
  let obj,_ = get_obj (Obj.set obj symb_prop_b (value_int 4) pc) in
  let obj,_ = get_obj (Obj.set obj symb_prop_c (value_int 5) pc) in

  let v = Obj.get obj symb_prop_z pc in

  Printf.printf "Obj State: %s\n" (Obj.to_string [(obj, value_bool true)]);
  Printf.printf "%s\n" (Smtml.Expr.to_string v)
