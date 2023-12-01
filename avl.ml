#load "btreeS.cmo";;
#load "useBtree.cmo";;
#load "bst.cmo";;
open UseBtree;;
open Bst;;

type 'a t_avl = 'a t_bst;;

let avl_isempty(avl : 'a t_avl) : bool =
  bst_isempty(avl)
;;

let avl_empty() : 'a t_avl =
  bst_empty()
;;

let avl_root(avl : 'a t_avl) : 'a =
  bst_root(avl)
;;

let avl_rooting(x, g, d : 'a * 'a t_avl * 'a t_avl) : 'a t_avl =
  bst_rooting(x, g, d)
;;

let avl_subleft(avl : 'a t_avl) : 'a t_avl =
  bst_subleft(avl)
;;

let avl_subright(avl : 'a t_avl) : 'a t_avl =
  bst_subright(avl)
;;

let rec avl_to_string(avl : string t_avl) : string =
  if avl_isempty(avl)
  then "EMPTY"
  else
  "(" ^ avl_root(avl) ^ "," ^ avl_to_string(avl_subleft(avl)) ^ "," ^ avl_to_string(avl_subright(avl)) ^ ")"
;;

let rec avl_to_string_int(avl : int t_avl) : string =
  if avl_isempty(avl)
  then "EMPTY"
  else
  "(" ^ string_of_int(avl_root(avl)) ^ "," ^ avl_to_string_int(avl_subleft(avl)) ^ "," ^ avl_to_string_int(avl_subright(avl)) ^ ")"
;;

let rec avl_to_string_int_int(avl : (int*int) t_avl) : string =
  if avl_isempty(avl)
  then "EMPTY"
  else
    let (r,imbalance) : int * int = avl_root(avl) in
  "(" ^  "(" ^ string_of_int(r) ^ "," ^ string_of_int(imbalance) ^ ")" ^ "," ^ avl_to_string_int_int(avl_subleft(avl)) ^ "," ^ avl_to_string_int_int(avl_subright(avl)) ^ ")"
;;

let avl_rg(t : 'a t_avl) : 'a t_avl =
  if avl_isempty(t)
  then failwith("avl_rg : avl is empty")
  else
    let root : 'a = avl_root(t)
    and (g, d) : 'a t_avl * 'a t_avl = (avl_subleft(t), avl_subright(t))
    in
    if avl_isempty(d)
    then failwith("avl_rg : avl subright is empty")
    else avl_rooting(avl_root(d), avl_rooting(root, g, avl_subleft(d)), avl_subright(d))
;;

let avl_rd(t : 'a t_avl) : 'a t_avl =
  if avl_isempty(t)
  then failwith("avl_rd : avl is empty")
  else
    let root : 'a = avl_root(t)
    and (g, d) : 'a t_avl * 'a t_avl = (avl_subleft(t), avl_subright(t))
    in
    if avl_isempty(g)
    then failwith("avl_rd : avl subleft is empty")
    else avl_rooting(avl_root(g), avl_subleft(g), avl_rooting(root, avl_subright(g), d))
;;

let avl_rdg(t : 'a t_avl) : 'a t_avl =
  if avl_isempty(t)
  then failwith("avl_rdg : avl is empty")
  else
    let root : 'a = avl_root(t)
    and (g, d) : 'a t_avl * 'a t_avl = (avl_subleft(t), avl_subright(t))
    in
    if avl_isempty(d)
    then failwith("avl_rdg : subright is empty")
    else
      let new_d : 'a t_avl = avl_rd(d) in
      avl_rg(avl_rooting(root, g, new_d))
;;

let avl_rgd(t : 'a t_avl) : 'a t_avl =
  if avl_isempty(t)
  then failwith("avl_rgd : avl is empty")
  else
    let root : 'a = avl_root(t)
    and (g, d) : 'a t_avl * 'a t_avl = (avl_subleft(t), avl_subright(t))
    in
    if avl_isempty(g)
    then failwith("avl_rgd : subleft is empty")
    else
      let new_g : 'a t_avl = avl_rg(g) in
      avl_rd(avl_rooting(root, new_g, d))
;;
(*
(* Création d'arbres spécifiques pour chaque type de rotation *)

(* Arbre pour la rotation à droite (avl_rg) *)
let tree_for_rg = bst_lbuild [4;2;5;1;3];;
let avl_tree_for_rg = avl_rooting (avl_root tree_for_rg, avl_subleft tree_for_rg, avl_subright tree_for_rg);;

(* Arbre pour la rotation à gauche (avl_rd) *)
let tree_for_rd = bst_lbuild [2;1;4;3;5];;
let avl_tree_for_rd = avl_rooting (avl_root tree_for_rd, avl_subleft tree_for_rd, avl_subright tree_for_rd);;

(* Arbre pour la rotation à droite-gauche (avl_rdg) *)
let tree_for_rdg = bst_lbuild [2;1;6;4;7;3;5];;
let avl_tree_for_rdg = avl_rooting (avl_root tree_for_rdg, avl_subleft tree_for_rdg, avl_subright tree_for_rdg);;

(* Arbre pour la rotation à gauche-droite (avl_rgd) *)
let tree_for_rgd = bst_lbuild [6;2;7;1;4;3;5];;
let avl_tree_for_rgd = avl_rooting (avl_root tree_for_rgd, avl_subleft tree_for_rgd, avl_subright tree_for_rgd);;

(* Affichage des arbres avant et après rééquilibrage *)

(* Avant rééquilibrage *)
print_endline ("AVL Tree for avl_rg (before): " ^ avl_to_string_int avl_tree_for_rg);;
print_endline ("AVL Tree for avl_rd (before): " ^ avl_to_string_int avl_tree_for_rd);;
print_endline ("AVL Tree for avl_rdg (before): " ^ avl_to_string_int avl_tree_for_rdg);;
print_endline ("AVL Tree for avl_rgd (before): " ^ avl_to_string_int avl_tree_for_rgd);;

(* Après rééquilibrage *)
let rg_tree_result = avl_rg avl_tree_for_rg;;
let rd_tree_result = avl_rd avl_tree_for_rd;;
let rdg_tree_result = avl_rdg avl_tree_for_rdg;;
let rgd_tree_result = avl_rgd avl_tree_for_rgd;;

print_endline ("AVL Tree for avl_rg (after): " ^ avl_to_string_int rg_tree_result);;
print_endline ("AVL Tree for avl_rd (after): " ^ avl_to_string_int rd_tree_result);;
print_endline ("AVL Tree for avl_rdg (after): " ^ avl_to_string_int rdg_tree_result);;
print_endline ("AVL Tree for avl_rgd (after): " ^ avl_to_string_int rgd_tree_result);;
*)

let rec avl_convert_imbalance(t : 'a t_avl) : ('a * int) t_avl =
  if avl_isempty(t)
  then failwith("Cannot imbalance an empty tree")
  else 
    let r : 'a = avl_root(t) in
    if avl_isempty(avl_subleft(t)) && avl_isempty(avl_subright(t))
    then avl_rooting((r, 0), avl_empty(), avl_empty())
    else 
      if avl_isempty(avl_subleft(t))
      then avl_rooting((r, bst_imbalance(t)), avl_empty(), avl_convert_imbalance(avl_subright(t)))
      else  
        if avl_isempty(avl_subright(t))
        then avl_rooting((r, bst_imbalance(t)), avl_convert_imbalance(avl_subleft(t)), avl_empty())
        else avl_rooting((r, bst_imbalance(t)), avl_convert_imbalance(avl_subleft(t)), avl_convert_imbalance(avl_subright(t)))
;;

let rec avl_recalcul_convert_imbalance(t : ('a * int) t_avl) : ('a * int) t_avl =
  if avl_isempty(t) 
  then failwith("Cannot imbalance an empty tree")
  else
  let (r, imbalance) : 'a * int = avl_root(t) in
  if avl_isempty(avl_subleft(t)) && avl_isempty(avl_subright(t))
    then avl_rooting((r, 0), avl_empty(), avl_empty())
    else 
      if avl_isempty(avl_subleft(t))
      then avl_rooting((r, bst_imbalance(t)), avl_empty(), avl_recalcul_convert_imbalance(avl_subright(t)))
      else  
        if avl_isempty(avl_subright(t))
        then avl_rooting((r, bst_imbalance(t)), avl_recalcul_convert_imbalance(avl_subleft(t)), avl_empty())
        else avl_rooting((r, bst_imbalance(t)), avl_recalcul_convert_imbalance(avl_subleft(t)), avl_recalcul_convert_imbalance(avl_subright(t)))
      ;;
(*let tree : int t_avl = bst_lbuild([1;2;3]);;
avl_to_string_int(tree);;
let imbalance_tree : (int*int) t_avl = avl_convert_imbalance(tree);;
avl_to_string_int_int(imbalance_tree);;*)

let avl_rebalance(t : 'a t_avl) : 'a t_avl =
  let imbalance : int = bst_imbalance(t) in
  if imbalance > 2 || imbalance < -2
  then failwith("avl_relance : too unbalanced")
  else
    if imbalance = -1 || imbalance = 0 || imbalance = 1
    then t
    else
      let imbalance_g : int = bst_imbalance(avl_subleft(t))
      and imbalance_d : int = bst_imbalance(avl_subright(t))
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

(*let tree : int t_avl = bst_lbuild([1;2;3]);;
avl_to_string_int(tree);;
let rebalance_tree : int t_avl = avl_rebalance(tree);;
avl_to_string_int(rebalance_tree);;*)
let tree : int t_avl = bst_lbuild([4;2;1;3]);;
avl_to_string_int(tree);;
let rebalance_tree : int t_avl = avl_rebalance(tree);;
avl_to_string_int(rebalance_tree)

(*
let avl_rebalance2(t : ('a * int) t_avl) : ('a * int) t_avl =
  let avl_rebalance_aux(t : ('a * int) t_avl) : ('a * int) t_avl =
    let (_, imbalance) : 'a * int = avl_root(t) in
    if imbalance > 2 || imbalance < -2
    then failwith("avl_relance : too unbalanced")
    else
      if imbalance = -1 || imbalance = 0 || imbalance = 1
      then t
      else
        let g : ('a * int) t_avl = avl_subleft(t)
        and d : ('a * int) t_avl = avl_subright(t)
        in
        if not(avl_isempty(g))
          then let (_,imbalance_g) = avl_root(g)
        in
        if not(avl_isempty(d))
          then let (_,imbalance_d) = avl_root(d)
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
  in 
  avl_recalcul_convert_imbalance(avl_rebalance_aux(t))
;;*)

(*let tree : int t_avl = bst_lbuild([1;0;2;3;4]);;
avl_to_string_int(tree);;
let imbalance_tree : (int * int) t_avl = avl_convert_imbalance(tree);;
avl_to_string_int_int(imbalance_tree);;
let rebalance_tree : (int * int) t_avl = avl_rebalance2(imbalance_tree);;
avl_to_string_int_int(rebalance_tree);; *)


let rec avl_add(tree, element :'a t_avl * 'a) : 'a t_avl =
  let empty : 'a t_avl = avl_empty()
  in
  if avl_isempty(tree)
  then avl_rooting(element, empty, empty)
  else
    let root : 'a = avl_root(tree)
    and (subleft, subright) : 'a t_avl * 'a t_avl = (avl_subleft(tree), avl_subright(tree))
    in
    if element < root
    then avl_rebalance(avl_rooting(root, avl_add(subleft, element), subright))
    else 
      if element > root
      then avl_rebalance(avl_rooting(root, subleft, avl_add(subright, element)))
      else avl_rooting(root, subleft, subright)
;;

(* let tree : int t_avl = avl_rebalance(bst_lbuild([3;2;1;4;8;6;7]));;
avl_to_string_int(tree);;
avl_to_string_int(avl_add(tree, 9));; *)

let rec avl_delete_max(tree : 'a t_avl) : 'a t_avl =
  if avl_isempty(tree)
  then failwith("avl_delete_max : AVL is empty")
  else
    let root : 'a = avl_root(tree)
    and (subleft, subright) : 'a t_avl * 'a t_avl = (avl_subleft(tree), avl_subright(tree))
    in
    if avl_isempty(subright)
    then subleft
    else avl_rebalance(avl_rooting(root, subleft, avl_delete_max(subright)))
;;

(* let tree : int t_avl = avl_lbuild([3;2;1;4;8;6;7]);; *)
let tree : int t_avl = avl_lbuild([5;4;3;2;1]);;
(* let tree : int t_avl = avl_lbuild([1;2;3;4;5]);; *)
avl_to_string_int(tree);;
let test_avl_delete_max = avl_delete_max(tree);;
avl_to_string_int(test_avl_delete_max);;
(* let test_avl_delete_7 = avl_delete(tree, 7);;
avl_to_string_int(test_avl_delete_7);;
let test_avl_delete_2 = avl_delete(tree, 2);;
avl_to_string_int(test_avl_delete_2);;
let test_avl_delete_6 = avl_delete(tree, 6);;
avl_to_string_int(test_avl_delete_6);;
let test_avl_delete_4 = avl_delete(tree, 4);;
avl_to_string_int(test_avl_delete_4);; *)

let test_add = avl_add(tree, 9);;
avl_to_string_int(test_add);;

let rec avl_delete(tree, element : 'a t_avl * 'a) : 'a t_avl =
  if avl_isempty(tree)
  then failwith("avl_delete : AVL is empty")
  else
    let root : 'a = avl_root(tree)
    and (subleft, subright) : 'a t_avl * 'a t_avl = (avl_subleft(tree), avl_subright(tree))
    in
    if element < root
    then avl_rebalance(avl_rooting(root, avl_delete(subleft, element), subright))
    else if element > root
    then avl_rebalance(avl_rooting(root, subleft, avl_delete(subright, element)))
    else if element = root && not(avl_isempty(subleft)) && not(avl_isempty(subright))
    then avl_rebalance(avl_rooting(bst_max(subleft), avl_delete_max(subleft), subright))
    else if element = root && not(avl_isempty(subright))
    then subright
    else subleft
;;


(* -------------------------------------------------------------------- *)
(* A VERIFIER *)
(* VOIR SI ON PEUT PAS PRENDRE LES FONCTIONS DE BST DIRECTEMENT EN AJOUTANT JUSTE LE AVL_REBALANCE A CHAQUE AJOUT *)


let rec avl_linsert(tree, element : 'a t_avl * 'a) : 'a t_avl =
  let empty : 'a t_avl = avl_empty()
  in
  if (avl_isempty(tree))
  then avl_rooting(element, empty, empty)
  else
    let root : 'a = avl_root(tree) in
    if (element < root)
    then
      (
        avl_rebalance(avl_rooting(root, avl_linsert(avl_subleft(tree), element), avl_subright(tree)));
      )
    else
      (
        avl_rebalance(avl_rooting(root, avl_subleft(tree), avl_linsert(avl_subright(tree), element)));
      )
;;

let rec avl_lbuild_aux(list, tree : 'a list * 'a t_avl) : 'a t_avl =
  if (list = [])
  then tree
  else avl_lbuild_aux(List.tl(list), avl_linsert(tree, List.hd(list)))
;;

let avl_lbuild(list : 'a list) : 'a t_avl =
  avl_lbuild_aux(list, avl_empty())
;;
*)
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

