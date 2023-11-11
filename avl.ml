#load "btreeS.cmo";;
#load "useBtree.cmo";;
#load "bst.cmo";;
open UseBtree;;
open Bst;;

type 'a t_avl = 'a t_bst;;

let rec avl_to_string(avl : string t_avl) : string =
  if bst_isempty(avl)
  then "EMPTY"
  else
  "(" ^ bst_root(avl) ^ "," ^ avl_to_string(bst_subleft(avl)) ^ "," ^ avl_to_string(bst_subright(avl)) ^ ")"
;;

let avl_rg(t : 'a t_avl) : 'a t_avl =
  if bst_isempty(t)
  then failwith("avl_rg : avl is empty")
  else
    let root : 'a = bst_root(t)
    and (g, d) : 'a t_bst * 'a t_bst = (bst_subleft(t), bst_subright(t))
    in
    if bst_isempty(d)
    then failwith("avl_rg : avl subright is empty")
    else bst_rooting(bst_root(d), bst_rooting(root, g, bst_subleft(d)), bst_subright(d))
;;

let avl_rd(t : 'a t_avl) : 'a t_avl =
  if bst_isempty(t)
  then failwith("avl_rd : avl is empty")
  else
    let root : 'a = bst_root(t)
    and (g, d) : 'a t_bst * 'a t_bst = (bst_subleft(t), bst_subright(t))
    in
    if bst_isempty(g)
    then failwith("avl_rg : avl subleft is empty")
    else bst_rooting(bst_root(g), bst_subleft(g), bst_rooting(root, bst_subright(g), d))
;;

let avl_rdg(t : 'a t_avl) : 'a t_avl =
  if bst_isempty(t)
  then failwith("avl_rdg : avl is empty")
  else
    let root : 'a = bst_root(t)
    and (g, d) : 'a t_bst * 'a t_bst = (bst_subleft(t), bst_subright(t))
    in
    let new_d : 'a t_bst = avl_rd(d) in
    avl_rg(bst_rooting(root, g, new_d))
;;

let avl_rgd(t : 'a t_avl) : 'a t_avl =
  if bst_isempty(t)
  then failwith("avl_rgd : avl is empty")
  else
    let root : 'a = bst_root(t)
    and (g, d) : 'a t_bst * 'a t_bst = (bst_subleft(t), bst_subright(t))
    in
    let new_g : 'a t_bst = avl_rg(g) in
    avl_rd(bst_rooting(root, new_g, d))
;;

let avl_rebalance(t : 'a t_avl) : 'a t_avl =
let imbalance : int = bst_imbalance(t) in
  if imbalance > 2 || imbalance < -2
  then failwith("avl_relance : too unbalanced")
  else
    if imbalance = -1 || imbalance = 0 || imbalance = 1
    then t
    else
      let imbalance_g : int = bst_imbalance(bst_subleft(t))
      and imbalance_d : int = bst_imbalance(bst_subright(t))
      in
      if imbalance = 2 && imbalance_g = 1
      then avl_rd(t)
      else
        if imbalance = 2 && imbalance_g = -1
        then avl_rgd(t)
        else
           if imbalance = -2 && imbalance_d = -1
           then avl_rg(t)
           else avl_rdg(t)
;;
