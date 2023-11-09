(* Binary tree *)
type 'a btreeS

(* Create an empty binary tree *)
val btreeS_empty :  unit -> 'a btreeS

(* create a binary tree from its root and its two subtrees *)
val btreeS_rooting : 'a * 'a btreeS * 'a btreeS -> 'a btreeS

(* Check if the binary tree is empty *)
val btreeS_isempty : 'a btreeS -> bool

(* Returns the root of the binary tree *)
val btreeS_root : 'a btreeS -> 'a

(* Returns the subleft tree of the binary tree *)
val btreeS_subleft : 'a btreeS -> 'a btreeS

(* Returns the subright tree of the binary tree *)
val btreeS_subright : 'a btreeS -> 'a btreeS
