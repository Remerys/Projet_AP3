open BtreeS;;

(* Give the size of a binary tree *)
val size : 'a btreeS -> int;;

(* Give the maximum between 2 values *)
val max : 'a * 'a -> 'a;;

(* Give the minimum between 2 values *)
val min : 'a * 'a -> 'a;;

(* Give the minimum in a binary tree *)
val btreeS_min : 'a btreeS -> 'a;;

(* Give the maximum in a binary tree *)
val btreeS_max : 'a btreeS -> 'a;;

(* Give the height of a binary tree *)
val height : 'a btreeS -> int;;

(* Give a string of a int binary tree *)
val btree_to_string_int : int btreeS -> string;;

(* Give a string of a string binary tree *)
val btree_to_string : string btreeS -> string;;