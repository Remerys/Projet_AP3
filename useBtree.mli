open BtreeS;;

(* Give the size of a binary tree *)
val size : 'a t_bt -> int;;

(* Give the maximum between 2 values *)
val max : 'a * 'a -> 'a;;

(* Give the minimum between 2 values *)
val min : 'a * 'a -> 'a;;

(* Give the minimum in a binary tree *)
val bt_min : 'a t_bt -> 'a;;

(* Give the maximum in a binary tree *)
val bt_max : 'a t_bt -> 'a;;

(* Give the height of a binary tree *)
val height : 'a t_bt -> int;;

(* Give a string of a int binary tree *)
val btree_to_string_int : int t_bt -> string;;

(* Give a string of a string binary tree *)
val btree_to_string : string t_bt -> string;;