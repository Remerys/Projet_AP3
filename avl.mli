open BtreeS;;
(* AVL tree *)
type 'a t_avl;;

val increment_counter : unit -> unit

val get_counter : unit -> int

val reset_counter : unit -> unit

(* Convert a AVL into a string *)
val avl_to_string : string t_avl -> string;;

val avl_to_string_int : 'a -> string

val avl_isempty : 'a -> bool

val avl_empty : unit -> 'a

val avl_root : 'b -> 'a

val avl_rooting : 'a * 'b * 'c -> 'd

val avl_subleft : 'a -> 'b

val avl_subright : 'a -> 'b

val avl_size : 'a -> int

(* Do an left rotation to the AVL*)
val avl_rg : 'a t_avl -> 'a t_avl;;

(* Do a right rotation to the AVL*)
val avl_rd  : 'a t_avl -> 'a t_avl;;

(* Do a right-left rotation to the AVL*)
val avl_rdg: 'a t_avl -> 'a t_avl ;;

(* Do a left-right rotation to the AVL*)
val avl_rgd : 'a t_avl -> 'a t_avl;;

(* Delete the max element of the AVL *)
val avl_delete_max : 'a t_avl * 'a -> 'a t_avl

(* Delete an element of the AVL *)
val avl_delete : 'a t_avl * 'a -> 'a t_avl

val avl_rebalance : 'a -> 'a

val avl_lbuild : 'a list -> 'b

val avl_add : 'a t_avl * 'a -> 'a t_avl;;

val avl_seek : 'b * 'a -> bool

val avl_rnd_create_aux : int -> int list

val avl_rnd_create : unit -> 'a

val avl_rnd_create2 : int -> 'a



val create_rnd_list : int -> int list

