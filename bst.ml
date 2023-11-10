#load "btreeS.cmo";;
#load "useBtree.cmo";;
open BtreeS;;
open UseBtree;;

type 'a t_bst = 'a btreeS;;

let bst_isempty(bst : 'a t_bst) : bool =
  btreeS_isempty(bst)
;;

let bst_empty() : 'a t_bst =
  btreeS_empty()
;;

let bst_root(bst : 'a t_bst) : 'a =
  btreeS_root(bst)
;;

let bst_rooting(x, g, d : 'a * 'a t_bst * 'a t_bst) : 'a t_bst =
  btreeS_rooting(x, g, d)
;;

let bst_subleft(bst : 'a t_bst) : 'a t_bst =
  btreeS_subleft(bst)
;;

let bst_subright(bst : 'a t_bst) : 'a t_bst =
  btreeS_subright(bst)
;;

let rec bst_linsert(bst, e : 'a t_bst * 'a) : 'a t_bst =
  if (bst_isempty(bst))
  then bst_rooting(e, bst_empty(), bst_empty())
  else
    let r : 'a = bst_root(bst) in
    if (e < r)
    then bst_rooting(r , bst_linsert(bst_subleft(bst), e), bst_subright(bst))
    else bst_rooting(r , bst_subleft(bst), bst_linsert(bst_subright(bst), e))
;;

let rec bst_lbuild_aux(l, t : 'a list * 'a t_bst) : 'a t_bst =
  if (l = [])
  then t
  else bst_lbuild_aux(List.tl(l), bst_linsert(t, List.hd(l)))
;;

let bst_lbuild(l : 'a list) : 'a t_bst =
  bst_lbuild_aux(l, bst_empty())
;;

let rec bst_isBst(bst : 'a t_bst) : bool =
  if bst_isempty(bst) (* bst empty *)
  then true
  else
    let (g, d) : 'a t_bst * 'a t_bst = (bst_subleft(bst), bst_subright(bst))
    and r : 'a = bst_root(bst)
    in
    if bst_isempty(g) && bst_isempty(d) (* leaf *)
    then true
    else
      if bst_isempty(g) (* bst left empty *)
      then
        if bst_root(d) < r
        then false
        else bst_isBst(d)
      else
        if bst_isempty(d) (* bst right empty *)
        then
          if bst_root(g) > r
          then false
          else bst_isBst(g)
        else
          if bst_root(g) > r || bst_root(d) < r (* 2 sub *)
          then false
          else bst_isBst(g) && bst_isBst(d)
;;


let no_bst : int btreeS = btreeS_rooting(2, btreeS_empty(), btreeS_rooting(1, btreeS_empty(), btreeS_empty()));;
let leaf : int t_bst = bst_lbuild([4;2;1;8;16]);;
btree_to_string(leaf);;
bst_isBst(no_bst);;


(* ------------------------------------------------------------------------------------------------------------------------------- *)

let rec bst_seek(tree, element : 'a t_bst * 'a) : 'a t_bst =
  let root = btreeS_root(tree)
   and subleft = btreeS_subleft(tree)
   and subright = btreeS_subright(tree)
  in
  if element = root
  then tree
  else if element < root
  then bst_seek(subleft, element)
  else bst_seek(subright, element)
;;

let rec bst_max(tree : 'a t_bst) : 'a =
  let root = btreeS_root(tree)
   and subright = btreeS_subright(tree)
  in
  if subright = btreeS_empty()
  then root
  else bst_max(subright)
;;

let rec bst_deletemax(tree : 'a t_bst) : 'a t_bst =
  if btreeS_subright(tree) = btreeS_empty()
  then btreeS_subleft(tree)
  else btreeS_rooting(btreeS_root(tree), btreeS_subleft(tree), bst_deletemax(btreeS_subright(tree)))
;;

let rec bst_delete(tree, element : 'a t_bst * 'a) : 'a t_bst =
  let root = btreeS_root(tree)
   and subleft = btreeS_subleft(tree)
   and subright = btreeS_subright(tree)
  in
  if btreeS_isempty(tree)
  then btreeS_empty()
  else if element < root
  then btreeS_rooting(root, bst_delete(subleft, element), subright)
  else if element > root
  then btreeS_rooting(root, subleft, bst_delete(subright, element))
  else if element = root
  then subleft
  else if element = root && not(btreeS_isempty(subright))
  then subright
  else btreeS_rooting(bst_max(subleft), bst_deletemax(subleft), subright)
;;