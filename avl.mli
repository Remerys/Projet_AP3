open BtreeS;;
(* AVL tree *)
type 'a t_avl;;

(* Check if the AVL is empty *)
val avl_isempty : 'a t_avl -> bool;;

(* Create an empty AVL *)
val avl_empty() : unit -> 'a t_avl;;

(* Returns the root of the AVL *)
val avl_root : 'a t_avl -> 'a;;

(* Create an AVL from its root and its two subtrees *)
val avl_rooting : 'a * 'a t_avl * 'a t_avl -> 'a t_avl;;

(* Returns the subleft tree of the AVL *)
val avl_subleft : 'a t_avl -> 'a t_avl;;

(* Returns the subright tree of the AVL *)
val avl_subright : 'a t_avl -> 'a t_avl;;

(* Returns the size of the AVL *)
val avl_size : 'a t_avl -> int;;

(* Convert a AVL into a string *)
val avl_to_string : string t_avl -> string;;

(* Do an left rotation to the AVL*)
val avl_rg : 'a t_avl -> 'a t_avl;;

(* Do a right rotation to the AVL*)
val avl_rd  : 'a t_avl -> 'a t_avl;;

(* Do a left-right rotation to the AVL*)
val avl_rgd : 'a t_avl -> 'a t_avl;;

(* Do a right-left rotation to the AVL*)
val avl_rdg: 'a t_avl -> 'a t_avl ;;

(* Rebalance the AVL *)
val avl_rebalance : 'a t_avl -> 'a t_avl;;

(* Add an element to the AVL *)
val avl_add :'a t_avl * 'a -> 'a t_avl;;

(* Delete the max element of the AVL *)
val avl_delete_max : 'a t_avl -> 'a t_avl;;

(* Delete an element of the AVL *)
val avl_delete : 'a t_avl * 'a -> 'a t_avl;;

(* Create an AVL with a list *)
val avl_lbuild : 'a list -> 'a t_avl;;

(* Create random AVL without the duplication *)
val avl_rnd_create2 : int -> int t_avl

(* Returns true if there is an element in the AVL *)
val avl_seek : 'a t_avl * 'a -> bool