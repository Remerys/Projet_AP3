#load "btreeS.cmo";;
#load "useBtree.cmo";;
#load "bst.cmo";;
open UseBtree;;
open Bst;;

type 'a t_avl = 'a t_bst;;

let counter : int ref = ref 0;;

let increment_counter() : unit =
  counter := !counter + 1
;;

let get_counter() : int =
  !counter
;;

let reset_counter() : unit =
  counter := 0
;;

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

let rec avl_size(avl : 'a t_avl) : int =
  if(avl_isempty(avl))
  then 0
  else 1 + avl_size(avl_subleft(avl)) + avl_size(avl_subright(avl))
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

(* 1.1 *)

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

let avl_rgd(t : 'a t_avl) : 'a t_avl =
  if avl_isempty(t)
  then failwith("avl_rgd : avl is empty")
  else
    let root : 'a = avl_root(t)
    and (g, d) : 'a t_avl * 'a t_avl = (avl_subleft(t), avl_subright(t))
    in
    if avl_isempty(g) || avl_isempty(avl_subright(g))
    then failwith("avl_rgd : subleft is empty or subright of subleft empty")
    else
      avl_rooting(avl_root(avl_subright(g)), 
                  avl_rooting(avl_root(g), avl_subleft(g), avl_subleft(avl_subright(g))),
                  avl_rooting(root, avl_subright(avl_subright(g)), d)
      )
;;

let avl_rdg(t : 'a t_avl) : 'a t_avl =
  if avl_isempty(t)
  then failwith("avl_rdg : avl is empty")
  else
    let root : 'a = avl_root(t)
    and (g, d) : 'a t_avl * 'a t_avl = (avl_subleft(t), avl_subright(t))
    in
    if avl_isempty(d) || avl_isempty(avl_subleft(d))
    then failwith("avl_rdg : subright is empty or subleft of subright empty")
    else
      avl_rooting(avl_root(avl_subleft(d)),
                  avl_rooting(root, g, avl_subleft(avl_subleft(d))),
                  avl_rooting(avl_root(d), avl_subright(avl_subleft(d)), avl_subright(d))
                )
;;

(* 1.2 *)

let avl_rebalance(t : 'a t_avl) : 'a t_avl =
  let imbalance : int = bst_imbalance(t) in
  if imbalance > 2 || imbalance < -2
  then failwith("avl_relance : too unbalanced")
  else
    if imbalance = -1 || imbalance = 0 || imbalance = 1
    then t
    else
      let imbalance_g : int = bst_imbalance(avl_subleft(t))
      in
      if imbalance = 2 && (imbalance_g = 1 || imbalance_g = 0)
      then
      (
        increment_counter();
        avl_rd(t)
      )
      else
        if imbalance = 2 && imbalance_g = -1
        then
        (
          increment_counter();
          avl_rgd(t)
        )
        else
          let imbalance_d : int = bst_imbalance(avl_subright(t)) in
          if imbalance = -2 && (imbalance_d = -1 || imbalance_d = 0) 
          then
          (
            increment_counter();
            avl_rg(t)
          )
          else 
          (
            increment_counter();
            avl_rdg(t)
          )
;;

(* 1.3 *)

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

let rec avl_delete_max(tree : 'a t_avl) : 'a t_avl =
  if avl_isempty(tree)
  then tree
  else
    let root : 'a = avl_root(tree)
    and (subleft, subright) : 'a t_avl * 'a t_avl = (avl_subleft(tree), avl_subright(tree))
    in
    if avl_isempty(subright)
    then subleft
    else avl_rebalance(avl_rooting(root, subleft, avl_delete_max(subright)))
;;

let rec avl_delete(tree, element : 'a t_avl * 'a) : 'a t_avl =
  if avl_isempty(tree)
  then tree
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

let rec avl_seek(tree, element : 'a t_avl * 'a) : bool =
  if avl_isempty(tree)
  then false
  else
  let root = avl_root(tree)
  and subleft = avl_subleft(tree)
  and subright = avl_subright(tree)
  in
  if element = root
  then true
  else if element < root
  then avl_seek(subleft, element)
  else avl_seek(subright, element)
;;

let rec avl_lbuild_aux(list, tree : 'a list * 'a t_avl) : 'a t_avl =
  if (list = [])
  then tree
  else avl_lbuild_aux(List.tl(list), avl_add(tree, List.hd(list)))
;;

let avl_lbuild(list : 'a list) : 'a t_avl =
  avl_lbuild_aux(list, avl_empty())
;;

(* 2.1 *)

let avl_rnd_create_aux(list_length : int) : int list =
  let borne : int = power(2, 8) - 1 (* -1 sinon le module Random ne prend pas en compte l'argument *)
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

let avl_rnd_create2(list_lenght : int) : int t_avl =
  let rec avl_rnd_create2_aux(tree, size, list_lenght : int t_avl * int * int) : int t_avl =
    if list_lenght = 0
    then tree 
    else
      let random_list : 'a list = avl_rnd_create_aux(list_lenght) 
      in
      let tree : 'a t_avl = avl_lbuild_aux(random_list, tree)
      in
      let t_size : int = avl_size(tree) in
      avl_rnd_create2_aux(tree, t_size, list_lenght - (t_size - size))
  in
  avl_rnd_create2_aux(avl_empty(), 0, list_lenght)
;;


