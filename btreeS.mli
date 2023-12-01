(* Binary tree *)
type 'a t_bt

(* Create an empty binary tree *)
val bt_empty :  unit -> 'a t_bt

(* create a binary tree from its root and its two sut_bt *)
val bt_rooting : 'a * 'a t_bt * 'a t_bt -> 'a t_bt

(* Check if the binary tree is empty *)
val bt_isempty : 'a t_bt -> bool

(* Returns the root of the binary tree *)
val bt_root : 'a t_bt -> 'a

(* Returns the subleft tree of the binary tree *)
val bt_subleft : 'a t_bt -> 'a t_bt

(* Returns the subright tree of the binary tree *)
val bt_subright : 'a t_bt -> 'a t_bt
