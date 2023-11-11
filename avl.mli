open BtreeS;;
(* AVL tree *)
type 'a t_avl;;

(* Convert a AVL into a string *)
val avl_to_string : string t_avl -> string;;

(* Do an left rotation to the AVL*)
val avl_rg : 'a t_avl -> 'a t_avl;;

(* Do a right rotation to the AVL*)
val avl_rd  'a t_avl -> 'a t_avl;;

(* Do a right-left rotation to the AVL*)
val avl_rdg: 'a t_avl -> 'a t_avl ;;

(* Do a left-right rotation to the AVL*)
val avl_rgd : 'a t_avl -> 'a t_avl;;

(* Delete the max element of the AVL *)
val avl_delete_max : 'a t_avl * 'a -> 'a t_avl

(* Delete an element of the AVL *)
val avl_delete : 'a t_avl * 'a -> 'a t_avl
