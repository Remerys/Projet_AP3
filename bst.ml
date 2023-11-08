#load "btreeS.cmo";;
#load "useBtree.cmo";;
open BtreeS;;
open UseBtree;;

type 'a t_bst = 'a btreeS;; 

let rec bst_linsert(bst, e : 'a t_bst * 'a) : 'a t_bst =
  if (btreeS_isempty(bst))
  then btreeS_rooting(e, btreeS_empty(), btreeS_empty())
  else
    let r : 'a = btreeS_root(bst) in
    if (e < r)
    then btreeS_rooting(r , bst_linsert(btreeS_subleft(bst), e), btreeS_subright(bst))
    else btreeS_rooting(r , btreeS_subleft(bst), bst_linsert(btreeS_subright(bst), e))
;;

let bst_lbuild(l : 'a list) : 'a t_bst =
  bst_lbuild_aux(l, btreeS_empty())
;;


let rec bst_lbuild_aux(l, t : 'a list * 'a t_bst) : 'a t_bst =
  if (l = [])
  then t
  else bst_lbuild(List.tl(l), bst_linsert(t, List.hd(l))
;;

let leaf : int t_bst = bst_lbuild([1;2;4;8;16]);;
btree_to_string(leaf);;


