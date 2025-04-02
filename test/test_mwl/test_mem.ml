open Test.Utils
open Memory_models
module Heap = Memory_mwl3.M

let () =

  let pc = value_bool true in

  let heap = Heap.create () in
  let l1 = Heap.alloc heap "l1" in
  let l2 = Heap.alloc heap "l2" in

  let symb_prop_z = key_s "#z" in
  (* let symb_prop_y = key_s "#y" in *)
  let symb_w = key_s_int "#w" in
 
  let l = (ite (eq symb_w (value_int 0)) l1 l2) in
  let heaps = Heap.set heap l symb_prop_z (value_int 1) pc in
  Printf.printf "Heap State1:\n%s\n" (Heap.to_string heaps);

  let heap, _  = List.nth heaps 0 in
  let v = Heap.get heap l symb_prop_z pc in
  Printf.printf "%s\n" (Smtml.Expr.to_string v)



  (* let heaps = Heap.set heap l1 symb_prop_y (value_int 2) pc in *)

  (* Printf.printf "Heap State2:\n%s\n" (Heap.to_string heaps); *)