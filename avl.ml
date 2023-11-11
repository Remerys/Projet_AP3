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

let avl_add(tree, element :'a t_avl * 'a) : 'a t_avl =
  let empty : 'a t_bst = bst_empty()
  in
  if bst_isempty(tree)
  then bst_rooting(element, empty, empty)
  else
    let root : 'a = bst_root(tree)
    and (subleft, subright) : 'a t_bst * 'a t_bst = (bst_subleft(tree), bst_subright(tree))
    in
    if element < root
    then avl_rebalance(bst_rooting(root, avl_add(subleft, subright), subright))
    else if element > root
    then avl_rebalance(bst_rooting(root, subleft, avl_add(subright, element)))
    else bst_rooting(root, subleft, subright)
;;

let avl_delete_max(tree : 'a t_avl) : 'a t_avl =
  if bst_isempty(tree)
  then failwith("avl_delete_max : AVL is empty")
  else
    let root : 'a = bst_root(tree)
    and (subleft, subright) : 'a t_bst * 'a t_bst = (bst_subleft(tree), bst_subright(tree))
    in
    if bst_isempty(subright)
    then subleft
    else avl_rebalance(bst_rooting(root, subleft, avl_delete_max(subright)))
;;

let avl_delete(tree, element : 'a t_avl * 'a) : 'a t_avl =
  if bst_isempty(tree)
  then failwith("avl_delete : AVL is empty")
  else
    let root : 'a = bst_root(tree)
    and (subleft, subright) : 'a t_bst * 'a t_bst = (bst_subleft(tree), bst_subright(tree))
    in
    if element < root
    then avl_rebalance(bst_rooting(root, avl_delete(subleft, element), subright))
    else if element > root
    then avl_rebalance(bst_rooting(root, subleft, avl_delete(subright, element)))
    else if root && not(bst_isemptysubleft) && not(bst_isempty(subright))
    then avl_rebalance(bst_rooting(max(subleft), avl_delete_max(subleft), subright))
    else if element = root && not(bst_isempty(subright))
    then subright
    else subleft
;;


(* -------------------------------------------------------------------- *)
(* A VERIFIER *)
(* VOIR SI ON PEUT PAS PRENDRE LES FONCTIONS DE BST DIRECTEMENT EN AJOUTANT JUSTE LE AVL_REBALANCE A CHAQUE AJOUT *)


let rec avl_linsert(tree, element : 'a t_avl * 'a) : 'a t_avl =
  let empty : 'a t_bst = bst_empty()
  in
  if (bst_isempty(tree))
  then bst_rooting(element, empty, empty)
  else
    let root : 'a = bst_root(tree) in
    if (element < root)
    then
      bst_rooting(root, bst_linsert(bst_subleft(tree), element), bst_subright(tree));
      avl_rebalance(tree);
    else
      bst_rooting(root, bst_subleft(tree), bst_linsert(bst_subright(tree), element));
      avl_rebalance(tree);
;;

let rec avl_lbuild_aux(list, tree : 'a list * 'a t_avl) : 'a t_avl =
  if (list = [])
  then tree
  else bst_lbuild_aux(List.tl(list), bst_linsert(tree, List.hd(list)))
;;

let avl_lbuild(list : 'a list) : 'a t_avl =
  avl_lbuild_aux(list, bst_empty())
;;

let avl_rnd_create_aux(list_length : int) : int list =
  let borne : int = power(2, 30) - 1 (* -1 sinon le module Random ne prend pas en compte l'argument *)
  in
  let rec aux(n, list) : int list =
    if n = 0
    then list
    else
      let random_number = Random.int(borne) (* Générer un nombre aléatoire entre 0 et 1 073 741 822 *)
      in
      aux((n - 1), (random_number::list))
  in
  List.rev(aux(list_length, []))
;;

let avl_rnd_create() : int t_avl =
  let random_number : int = Random.int(200) (* Générer un nombre aléatoire entre 0 et 199 *)
  in
  let random_list : 'a list = avl_rnd_create_aux(random_number)
  in
  avl_lbuild(random_list)
;;
