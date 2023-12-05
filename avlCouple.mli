open BtreeS;;
type 'a t_avl;;

val avl_isempty : 'a -> bool
val avl_empty : unit -> 'a t_avl
val avl_root : 'a t_avl -> 'a
val avl_rooting : 'a * 'a t_avl * 'a t_avl -> 'a t_avl
val avl_subleft : 'a t_avl -> 'a t_avl
val avl_subright : 'a t_avl -> 'a t_avl
val avl_size : 'a t_avl -> int
val avl_to_string : 'a t_avl -> string
val avl_to_string_int_int : 'a t_avl -> string
val avl_rg : 'a t_avl -> 'a t_avl
val avl_rd : 'a t_avl -> 'a t_avl
val avl_rgd : 'a t_avl -> 'a t_avl
val avl_rdg : 'a t_avl -> 'a t_avl
val avl_rebalance : 'a t_avl -> 'a t_avl
