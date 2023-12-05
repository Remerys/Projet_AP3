#load "btreeS.cmo";;
#load "useBtree.cmo";;
#load "bst.cmo";;
open UseBtree;;
open Bst;;

type 'a t_avl = ('a * int) t_bst;;

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

let avl_rebalance(t : ('a * int) t_avl) : ('a * int) t_avl =
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
      let imbalance_g =
        if avl_isempty(g) then 0
        else let (_, imbalance_g) = avl_root(g) in imbalance_g
      in
      let imbalance_d =
        if avl_isempty(d) then 0
        else let (_, imbalance_d) = avl_root(d) in imbalance_d
      in
      if imbalance = 2 && (imbalance_g = 1 || imbalance_g = 0)
        then 
          let new_t = avl_rd(t) in 
          let new_g = avl_subleft(new_t) in
          let new_d = avl_subright(new_t) in
          let (r,imbalance) = avl_root(new_t) in 
          let (r_g, imbalance_g) = avl_root(new_g) in
          let (r_d, imbalance_d) = avl_root(new_d) in
          let new_imbalance = bst_imbalance(new_t) in 
          let new_imbalance_g = bst_imbalance(new_g) in 
          let new_imbalance_d = bst_imbalance(new_d) in 
          avl_rooting((r, new_imbalance), 
                    avl_rooting((r_g, new_imbalance_g), avl_subleft(new_g), avl_subright(new_g)), 
                    avl_rooting((r_d, new_imbalance_d), avl_subleft(new_d), avl_subright(new_d)))
        else
          if imbalance = 2 && imbalance_g = -1
          then 
            let new_t = avl_rgd(t) in 
            let new_gd = avl_subright(avl_subleft(new_t)) in
            let new_d = avl_subright(new_t) in
            let (r,imbalance) = avl_root(new_t) in 
            let (r_gd, imbalance_gd) = avl_root(new_gd) in
            let (r_d, imbalance_d) = avl_root(new_d) in
            let new_imbalance = bst_imbalance(new_t) in 
            let new_imbalance_gd = bst_imbalance(new_gd) in (*aled*)
            let new_imbalance_d = bst_imbalance(new_d) in
            avl_rooting((r, new_imbalance), 
                          avl_rooting(avl_root(avl_subleft(new_t)), 
                                avl_subleft(avl_subleft(new_t)), 
                                avl_rooting((r_gd, new_imbalance_gd), avl_subleft(new_gd), avl_subright(new_gd))),
                          avl_rooting((r_d, new_imbalance_d), avl_subleft(new_d), avl_subright(new_d))
                          )
          else
              if imbalance = -2 && (imbalance_d = -1 || imbalance_d = 0)
              then 
              let new_t = avl_rg(t) in 
              let new_g = avl_subleft(new_t) in
              let new_d = avl_subright(new_t) in
              let (r,imbalance) = avl_root(new_t) in 
              let (r_g, imbalance_g) = avl_root(new_g) in
              let (r_d, imbalance_d) = avl_root(new_d) in
              let new_imbalance = bst_imbalance(new_t) in 
              let new_imbalance_g = bst_imbalance(new_g) in 
              let new_imbalance_d = bst_imbalance(new_d) in 
              avl_rooting((r, new_imbalance), 
                        avl_rooting((r_g, new_imbalance_g), avl_subleft(new_g), avl_subright(new_g)), 
                        avl_rooting((r_d, new_imbalance_d), avl_subleft(new_d), avl_subright(new_d)))
              else 
              let new_t = avl_rdg(t) in 
              let new_dg = avl_subleft(avl_subright(new_t)) in
              let new_g = avl_subleft(new_t) in
              let (r,imbalance) = avl_root(new_t) in 
              let (r_dg, imbalance_dg) = avl_root(new_dg) in
              let (r_g, imbalance_g) = avl_root(new_g) in
              let new_imbalance = bst_imbalance(new_t) in 
              let new_imbalance_dg = bst_imbalance(new_dg) in (*aled*)
              let new_imbalance_g = bst_imbalance(new_g) in 
              avl_rooting((r, new_imbalance), 
                            avl_rooting((r_g, new_imbalance_g), avl_subleft(new_g), avl_subright(new_g)), 
                            avl_rooting(avl_root(avl_subright(new_t)), 
                                  avl_rooting((r_dg, new_imbalance_dg), avl_subleft(new_dg), avl_subright(new_dg)), 
                                  avl_subright(avl_subright(new_t))))
;;

let rec avl_add(tree, element : ('a * int) t_avl * 'a) : ('a * int) t_avl =
  let empty : ('a * int) t_avl = avl_empty()
  in
  if avl_isempty(tree)
  then avl_rooting((element,0), empty, empty)
  else
    let (root, imbalance) : 'a * int = (avl_root(tree))
    and (subleft, subright) : ('a * int) t_avl * ('a * int) t_avl = (avl_subleft(tree), avl_subright(tree))
    in
    if element < root
    then avl_rebalance(avl_recalcul_convert_imbalance(avl_rooting((root, imbalance), avl_add2(subleft, element), subright)))
    else
      if element > root
      then avl_rebalance(avl_recalcul_convert_imbalance(avl_rooting((root, imbalance), subleft, avl_add2(subright, element))))
      else avl_rooting((root, imbalance), subleft, subright) 
;;

let rec avl_delete_max(tree : ('a * int) t_avl) : ('a * int) t_avl =
  if avl_isempty(tree)
  then tree
  else
    let (root, imbalance) : 'a * int = (avl_root(tree))
    and (subleft, subright) : ('a * int) t_avl * ('a * int) t_avl = (avl_subleft(tree), avl_subright(tree))
    in
    if avl_isempty(subright)
    then subleft
    else avl_rebalance(avl_recalcul_convert_imbalance(avl_rooting((root, imbalance), subleft, avl_delete_max(subright))))
;;

let rec avl_delete(tree, element : ('a * int) t_avl * 'a) : ('a * int) t_avl =
  if avl_isempty(tree)
  then tree
  else
    let (root,imbalance) : 'a * int = avl_root(tree)
    and (subleft, subright) : ('a * int) t_avl * ('a * int) t_avl = (avl_subleft(tree), avl_subright(tree))
    in
    if element < root
    then avl_rebalance(avl_recalcul_convert_imbalance(avl_rooting((root, imbalance), avl_delete(subleft, element), subright)))
    else if element > root
    then avl_rebalance(avl_recalcul_convert_imbalance(avl_rooting((root, imbalance), subleft, avl_delete(subright, element))))
    else if element = root && not(avl_isempty(subleft)) && not(avl_isempty(subright))
    then avl_rebalance(avl_recalcul_convert_imbalance(avl_rooting(bst_max(subleft), avl_delete_max(subleft), subright)))
    else if element = root && not(avl_isempty(subright))
    then subright
    else subleft
;;
