(* Binary Search Tree *)
type 'a t_bst;;

(* Check if the binary search tree is empty *)
val bst_isempty : 'a t_bst -> bool

(* Create an empty binary search tree *)
val bst_empty() -> 'a t_bst

(* Returns the root of the binary search tree *)
val bst_root : 'a t_bst -> 'a

(* create a binary search tree from its root and its two subtrees *)
val bst_rooting : 'a * 'a t_bst * 'a t_bst -> 'a t_bst

(* Returns the subleft tree of the binary search tree *)
val bst_subleft : 'a t_bst -> 'a t_bst

(* Returns the subright tree of the binary search tree *)
val bst_subright : 'a t_bst -> 'a t_bst

(* Add an element to the leaf *)
val rec bst_linsert : 'a t_bst * 'a -> 'a t_bst ;;

(* Builds a binary search tree from a list *)
val bst_lbuild : 'a list -> 'a t_bst;;

(* Check if the tree is a binary search tree *)
val rec bst_isBst : 'a t_bst -> bool

(* Search an element in a binary search tree *)
val rec bst_seek : 'a t_bst * 'a -> 'a t_bst;;

(* Return the max element of a binary search tree *)
val rec bst_max : 'a t_bst -> 'a;;

(* Delete the max element of a binary search tree *)
val rec bst_deletemax : 'a t_bst -> 'a t_bst;;

(* Delete an element of a binary search tree *)
val rec bst_delete : 'a t_bst * 'a -> 'a t_bst;;
