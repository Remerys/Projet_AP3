type 'a btreeS = BT_EMPTY
               | BT_ROOTING of 'a * 'a btreeS * 'a btreeS
;;

let btreeS_empty() = BT_EMPTY;;

let btreeS_rooting(x, g, d : 'a * 'a btreeS * 'a btreeS) : 'a btreeS =
  BT_ROOTING(x, g, d);;

let btreeS_isempty(bt_tree : 'a btreeS) : bool =
  match bt_tree with
  | BT_EMPTY -> true
  | BT_ROOTING(x, g, d) ->false
;;

let btreeS_root(bt_tree : 'a btreeS) : 'a =
  match bt_tree with
  | BT_EMPTY -> failwith("root : L'arbre est vide")
  | BT_ROOTING(x, g, d) -> x
;;

let btreeS_subleft(bt_tree : 'a btreeS) : 'a btreeS =
  match bt_tree with
  | BT_EMPTY -> BT_EMPTY
  | BT_ROOTING(x, g, d) -> g
;;

let btreeS_subright(bt_tree : 'a btreeS) : 'a btreeS =
  match bt_tree with
  | BT_EMPTY -> BT_EMPTY
  | BT_ROOTING(x, g, d) -> d
;;






