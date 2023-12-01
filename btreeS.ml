type 'a t_bt = BT_EMPTY
               | BT_ROOTING of 'a * 'a t_bt * 'a t_bt
;;

let bt_empty() = BT_EMPTY;;

let bt_rooting(x, g, d : 'a * 'a t_bt * 'a t_bt) : 'a t_bt =
  BT_ROOTING(x, g, d);;

let bt_isempty(bt_tree : 'a t_bt) : bool =
  match bt_tree with
  | BT_EMPTY -> true
  | BT_ROOTING(x, g, d) ->false
;;

let bt_root(bt_tree : 'a t_bt) : 'a =
  match bt_tree with
  | BT_EMPTY -> failwith("root : L'arbre est vide")
  | BT_ROOTING(x, g, d) -> x
;;

let bt_subleft(bt_tree : 'a t_bt) : 'a t_bt =
  match bt_tree with
  | BT_EMPTY -> BT_EMPTY
  | BT_ROOTING(x, g, d) -> g
;;

let bt_subright(bt_tree : 'a t_bt) : 'a t_bt =
  match bt_tree with
  | BT_EMPTY -> BT_EMPTY
  | BT_ROOTING(x, g, d) -> d
;;






