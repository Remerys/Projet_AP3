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
          if not(btreeS_isempty(subleft)) && not(btreeS_isempty(subright))
          then btreeS_rooting(bst_max(subleft), bst_deletemax(subleft), subright)
          else
            if not(btreeS_isempty(subright))
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
        if btreeS_min(d) < r
        then false
        else bst_isBst(d)
      else
        if bst_isempty(d) (* bst right empty *)
        then
          if btreeS_max(g) > r
          then false
          else bst_isBst(g)
        else
          if btreeS_max(g) > r || btreeS_min(d) < r (* 2 subtree *)
          then false
          else bst_isBst(g) && bst_isBst(d)
;;

let rec power (number, exponent : 'a * 'a) : 'a =
  if exponent = 0
  then 1
  else number * power (number, exponent - 1)
;;

let rec create_random_list(list_length, list : 'a * 'a list) : 'a list =
  let borne : int = power(2, 30) - 1 (* -1 sinon le module Random ne prend pas en compte l'argument *)
  in
  let random_number : int = Random.int(borne) (* Générer un nombre aléatoire entre 0 et 1 073 741 822 *)
  in
  if list_length = 0
  then list
  else create_random_list(list_length-1, random_number::list)
;;

let bst_rnd_create() : 'a t_bst =
  let random_number : int = Random.int(200) (* Générer un nombre aléatoire entre 0 et 199 *)
  in
  let random_list : 'a list = create_random_list(random_number, [])
  in
  bst_lbuild(random_list)
;;

let imbalance_tree(tree : 'a t_bst) : 'a =
  if bst_isempty(tree)
  then 0
  else
    let subleft : 'a t_bst = bst_subleft(tree)
    and subright : 'a t_bst = bst_subright(tree)
    in
    if bst_isempty(tree)
    then 0
    else height(subleft) - height(subright)
;;

let test() : float =
  let sample : int = 10000
  and average_imbalance : int ref = ref 0
  in
  for i = 1 to sample do
    let tree : 'a t_bst = bst_rnd_create()
    in
    average_imbalance := !average_imbalance + imbalance_tree(tree)
  done;

  let res : float = float_of_int(!average_imbalance)/.float_of_int(sample) in
  res
;;

let test2() : float list =
  let res : float list ref = ref []
  in
  for i = 0 to 50 do
    res := test()::!res
  done;
  !res
;;
