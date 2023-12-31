(* Binary Search Tree *)
type 'a t_bst = 'a BtreeS.t_bt;;

(* Check if the binary search tree is empty *)
val bst_isempty : 'a t_bst -> bool;;

(* Create an empty binary search tree *)
val bst_empty : unit -> 'a t_bst;;

(* Returns the root of the binary search tree *)
val bst_root : 'a t_bst -> 'a;;

(* Create a binary search tree from its root and its two subtrees *)
val bst_rooting : 'a * 'a t_bst * 'a t_bst -> 'a t_bst;;

(* Returns the subleft tree of the binary search tree *)
val bst_subleft : 'a t_bst -> 'a t_bst;;

(* Returns the subright tree of the binary search tree *)
val bst_subright : 'a t_bst -> 'a t_bst;;

val bst_height : 'a t_bst -> int;;

(* Add an element to the leaf *)
val bst_linsert : 'a t_bst * 'a -> 'a t_bst;;

(* Builds a binary search tree from a list *)
val bst_lbuild : 'a list -> 'a t_bst;;

(* Check if the tree is a binary search tree *)
val bst_isBst : 'a t_bst -> bool;;

(* Search an element in a binary search tree *)
val bst_seek : 'a t_bst * 'a -> bool;;

(* Return the min element of a binary search tree *)
val bst_min : 'a t_bst -> 'a;;

(* Return the max element of a binary search tree *)
val bst_max : 'a t_bst -> 'a;;

(* Delete the max element of a binary search tree *)
val bst_deletemax : 'a t_bst -> 'a t_bst;;

(* Delete an element of a binary search tree *)
val bst_delete : 'a t_bst * 'a -> 'a t_bst;;

(* Verify if the binary tree is a binary search tree *)
val bst_isBst : 'a t_bst -> bool;;

(* Calculate the power of a number *)
val power : int * int -> int;;

(* Create a binary search tree *)
val bst_rnd_create : unit -> int t_bst;;

(* Create a binary search tree based on a given size*)
val bst_create_rnd_tree  : int -> int t_bst;;

(* Check the imbalance of a binary search tree *)
val bst_imbalance : 'a t_bst -> int;;

(* Calculate the average imbalance of a created binary search tree *)
val bst_average_imbalance : unit -> float list;;

(* Calculate the average imbalance of a created binary search tree *)
val bst_average_imbalance_aux : unit -> float;;

(* Calculate the average imbalance of binary search tree *)
val bst_average_imbalance_tree : 'a t_bst -> float;;

(* Create an list of a subserie*)
val subserie : int * int -> int list;;

(* Create an list of subseries*)
val subseries : int * int * int -> int list;;

(* Calculate the imbalance of a binary search tree with subseries*)
val bst_imbalance_subseries : int -> float;;

(* Calculate the average of imbulance of binary search trees with subseries with fixed size*)
val bst_average_imbalance_subseries : int -> float;;

(* Calculate the average of imbulance of binary search trees with subseries with random size*)
val bst_average_imbalance_subseries_random : unit -> float;;

(* Calculate the average of imbulance of binary search trees with subseries with increasing size*)
val bst_average_imbalance_subseries_increase : unit -> float;;

(* Calculate the average of imbulance of binary search trees with subseries with decreasing size*)
val bst_average_imbalance_subseries_decrease : unit -> float;;

