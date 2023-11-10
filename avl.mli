open BtreeS;;
(* AVL tree *)
type 'a t_avl;;

(* Convert a AVL into a string *)
let avl_to_string : string t_avl -> string;;

(* Do a left rotate to the AVL*)
let avl_rg : 'a t_avl -> 'a t_avl;;

(* Do a right rotate to the AVL*)
let avl_rd  'a t_avl -> 'a t_avl;;

(* Do a right-left rotate to the AVL*)
let avl_rdg: 'a t_avl -> 'a t_avl ;;

(* Do a left-right rotate to the AVL*)
let avl_rgd : 'a t_avl -> 'a t_avl;;
