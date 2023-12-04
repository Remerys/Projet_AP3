(*#load "btreeS.cmo";;
#load "useBtree.cmo";;*)
open BtreeS;;
open UseBtree;;

type 'a t_bst = 'a t_bt;;

let bst_isempty(bst : 'a t_bst) : bool =
  bt_isempty(bst)
;;

let bst_empty() : 'a t_bst =
  bt_empty()
;;

let bst_root(bst : 'a t_bst) : 'a =
  bt_root(bst)
;;

let bst_rooting(x, g, d : 'a * 'a t_bst * 'a t_bst) : 'a t_bst =
  bt_rooting(x, g, d)
;;

let bst_subleft(bst : 'a t_bst) : 'a t_bst =
  bt_subleft(bst)
;;

let bst_subright(bst : 'a t_bst) : 'a t_bst =
  bt_subright(bst)
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

let rec bst_seek(tree, element : 'a t_bst * 'a) : bool =
  if bst_isempty(tree)
  then false
  else
  let root = bst_root(tree)
  and subleft = bst_subleft(tree)
  and subright = bst_subright(tree)
  in
  if element = root
  then true
  else if element < root
  then bst_seek(subleft, element)
  else bst_seek(subright, element)
;;

let rec bst_min(tree : 'a t_bst) : 'a =
  if bst_isempty(tree)
  then failwith("bst_min : l'arbre est vide")
  else
  let root : 'a = bst_root(tree)
  and subleft : 'a t_bst = bst_subleft(tree)
  in
  if bst_isempty(subleft)
  then root
  else bst_min(subleft)
;;

let rec bst_max(tree : 'a t_bst) : 'a =
  if bst_isempty(tree)
  then failwith("bst_max : l'arbre est vide")
  else
  let root : 'a = bst_root(tree)
  and subright : 'a t_bst = bst_subright(tree)
  in
  if bst_isempty(subright)
  then root
  else bst_max(subright)
;;

let rec bst_deletemax(tree : 'a t_bst) : 'a t_bst =
 if bst_isempty(tree)
 then failwith("bst_deletemax : l'arbre est vide")
 else
   let r : 'a = bst_root(tree)
   and (g, d) : 'a t_bst * 'a t_bst = (bst_subleft(tree), bst_subright(tree))
   in
   if bst_isempty(d)
   then g
   else bst_rooting(r, g, bst_deletemax(d))
;;

let rec bst_delete(tree, element : 'a t_bst * 'a) : 'a t_bst =
  if bst_isempty(tree)
  then failwith("bst_deletemax : l'arbre est vide")
  else
    let r : 'a = bst_root(tree)
    and subleft : 'a t_bst = bst_subleft(tree)
    and subright : 'a t_bst = bst_subright(tree)
    in
    if bst_isempty(tree)
    then bst_empty()
    else
      if element < r
      then bst_rooting(r, bst_delete(subleft, element), subright)
      else
        if element > r
        then bst_rooting(r, subleft, bst_delete(subright, element))
        else
          if not(bt_isempty(subleft)) && not(bt_isempty(subright))
          then bt_rooting(bst_max(subleft), bst_deletemax(subleft), subright)
          else
            if not(bt_isempty(subright))
            then subright
            else subleft
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
        if bt_min(d) < r
        then false
        else bst_isBst(d)
      else
        if bst_isempty(d) (* bst right empty *)
        then
          if bt_max(g) > r
          then false
          else bst_isBst(g)
        else
          if bt_max(g) > r || bt_min(d) < r (* 2 subtree *)
          then false
          else bst_isBst(g) && bst_isBst(d)
;;

let rec power(number, exponent : int * int) : int =
  if exponent = 0 then 1
  else if exponent mod 2 = 0
  then
    let half_power = power (number, exponent / 2)
    in
    half_power * half_power
  else
    number * power (number, exponent - 1)
;;

(* 1 *)

let bst_rnd_create_aux(list_length : int) : int list =
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

let bst_rnd_create() : int t_bst =
  let random_number : int = Random.int(200) (* Générer un nombre aléatoire entre 0 et 199 *)
  in
  let random_list : 'a list = bst_rnd_create_aux(random_number)
  in
  bst_lbuild(random_list)
;;

let bst_create_rnd_list(size : int) : int list=
  let l : int list ref = ref [] in
  for i=0 to size do
    let r : int = Random.int(200) in
    l := r::!l
  done;
  !l
;;

let bst_create_rnd_tree(size : int) : int t_bst =
  bst_lbuild(bst_create_rnd_list(size))
;;


(* 2 *)

let bst_imbalance(tree : 'a t_bst) : int =
  if bst_isempty(tree)
  then 0
  else
    let subleft : 'a t_bst = bst_subleft(tree)
    and subright : 'a t_bst = bst_subright(tree)
    in
    height(subleft) - height(subright)
;;

let bst_average_imbalance_tree(t : 'a t_bst) : float = 
  let rec bst_sum_imbalance_tree(t : 'a t_bst) : int =
    if bst_isempty(t)
    then 0
    else bst_imbalance(t) + bst_sum_imbalance_tree(bst_subleft(t)) + bst_sum_imbalance_tree(bst_subright(t))
  in
  if bst_isempty(t)
  then 0.
  else float_of_int(bst_sum_imbalance_tree(t))/.float_of_int(size(t))
;;


let bst_average_imbalance_aux() : float =
  let sample : int = 10000
  and average_imbalance : float ref = ref 0.
  in
  for i = 1 to sample do
    let tree : 'a t_bst = bst_rnd_create()
    in
    average_imbalance := !average_imbalance +. bst_average_imbalance_tree(tree)
  done;

  let res : float = !average_imbalance/.float_of_int(sample) in
  res
;;

let bst_average_imbalance() : float list =
  let res : float list ref = ref []
  in
  for i = 0 to 10 do
    res := bst_average_imbalance_aux()::!res
  done;
  !res
;;

(* 3 *)

let subserie(size, max : int * int) : int list =
  let r : int = Random.int(max) in
  let rec subserie_aux(size, r, l : int * int * int list) : int list =
    if size = 0
    then l
    else subserie_aux(size-1, r, (r+size-1)::l)
  in
  subserie_aux(size, r, [])
;;

let rec subseries(size, sizeSub, max : int * int * int) : int list =
  if (size = 0)
  then []
  else subserie(sizeSub, max) @ subseries(size-1, sizeSub, max)
;;

let bst_imbalance_subseries(sizeSub : int) : float =
  let sample : int = 1000 in
  let imbalance : float ref = ref 0. in
  for i=0 to sample do
    let tree : 'a t_bst = bst_lbuild(subseries(10, sizeSub, 200)) in
      imbalance := !imbalance +. bst_average_imbalance_tree(tree)
  done;
  !imbalance/.float_of_int(sample)
;;

let bst_average_imbalance_subseries(sizeSub : int) : float =
  let imbalance : float ref = ref 0. in 
  for i=0 to 10 do 
    imbalance := (!imbalance) +. bst_imbalance_subseries(sizeSub)
  done;
  !imbalance/.10.
;;


let bst_average_imbalance_subseries_random() : float =
  let sizeSub : int = Random.int(10)
  in
  bst_average_imbalance_subseries(sizeSub);
;;

let bst_average_imbalance_subseries_increase() : float =
  let imbalance : float ref = ref 0. in 
  let sizeSub : int ref = ref 1 in
  for i=0 to 10 do 
    imbalance := (!imbalance) +. bst_imbalance_subseries(!sizeSub);
    sizeSub := (!sizeSub) + 1
  done;
  !imbalance/.10.
;;

let bst_average_imbalance_subseries_decrease() : float =
  let imbalance : float ref = ref 0. in 
  let sizeSub : int ref = ref 11 in
  for i=0 to 10 do 
    imbalance := (!imbalance) +. bst_imbalance_subseries(!sizeSub);
    sizeSub := (!sizeSub) - 1
  done;
  !imbalance/.10.
;;
(*
bst_average_imbalance_subseries(4);;
bst_average_imbalance_subseries_random();;
bst_average_imbalance_subseries_increase();;
bst_average_imbalance_subseries_decrease();;*)

