#use "avl.ml";;
(* #use "avlCouple.ml";; *)

(* ROTATION


(* Création d'arbres spécifiques pour chaque type de rotation *)

(* Arbre pour la rotation à droite (avl_rd) *)
let tree_for_rd = bst_lbuild [3;2;1];;
let avl_tree_for_rd = avl_rooting (avl_root tree_for_rd, avl_subleft tree_for_rd, avl_subright tree_for_rd);;

(* Arbre pour la rotation à gauche (avl_rd) *)
let tree_for_rg = bst_lbuild [1;2;3];;
let avl_tree_for_rg = avl_rooting (avl_root tree_for_rg, avl_subleft tree_for_rg, avl_subright tree_for_rg);;

(* Arbre pour la rotation à droite-gauche (avl_rdg) *)
let tree_for_rdg = bst_lbuild [1;3;2];;
let avl_tree_for_rdg = avl_rooting (avl_root tree_for_rdg, avl_subleft tree_for_rdg, avl_subright tree_for_rdg);;

(* Arbre pour la rotation à gauche-droite (avl_rgd) *)
let tree_for_rgd = bst_lbuild [3;1;2];;
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
print_endline ("AVL Tree for avl_rgd (after): " ^ avl_to_string_int rgd_tree_result);; *)


(* REBALANCE
let tree : int t_avl = bst_lbuild([1;2;3]);;
avl_to_string_int(tree);;
let rebalance_tree : int t_avl = avl_rebalance(tree);;
avl_to_string_int(rebalance_tree);;*)
(*let tree : int t_avl = bst_lbuild([4;2;1;3]);;
avl_to_string_int(tree);;*)
(*let tree : int t_avl = bst_lbuild([3;2;1;5]);;
avl_to_string_int(tree);;
let rebalance_tree : int t_avl = avl_rebalance(tree);;
avl_to_string_int(rebalance_tree)*)


(* let tree : int t_avl = bst_lbuild([5;2;8;6;9;7]);;
avl_to_string_int(tree);;
let imbalance_tree : (int * int) t_avl = avl_convert_imbalance(tree);;
avl_to_string_int_int(imbalance_tree);;
let rebalance_tree : (int * int) t_avl = avl_rebalance2(imbalance_tree);;
avl_to_string_int_int(rebalance_tree);; *)

(* RANDOM 
let test_rnd_create2 = avl_rnd_create2(50);;
avl_to_string_int(test_rnd_create2);;
avl_size(test_rnd_create2);; *)










(* TESTS AVL_RND_CREATE *)

(* TEST 1 *)
(* let test_avl_rnd_create1 = avl_rnd_create();;
avl_to_string_int(test_avl_rnd_create1);; *)

(* END TESTS AVL_RND_CREATE *)



(* TESTS SEEK *)

(* TEST 1 *)
(* let tree : int t_avl = avl_lbuild([3;2;1;4;8;6;7]);;
let test_seek_8 = avl_seek(tree, 8);;
let test_seek_7 = avl_seek(tree, 7);;
let test_seek_6 = avl_seek(tree, 6);;
let test_seek_5 = avl_seek(tree, 5);;
let test_seek_4 = avl_seek(tree, 4);;
let test_seek_3 = avl_seek(tree, 3);;
let test_seek_2 = avl_seek(tree, 2);;
let test_seek_1 = avl_seek(tree, 1);; *)

(* TEST 2 *)
(* let tree : int t_avl = avl_lbuild([5;4;3;2;1]);;
let test_seek_6 = avl_seek(tree, 6);;
let test_seek_5 = avl_seek(tree, 5);;
let test_seek_4 = avl_seek(tree, 4);;
let test_seek_3 = avl_seek(tree, 3);;
let test_seek_2 = avl_seek(tree, 2);;
let test_seek_1 = avl_seek(tree, 1);; *)

(* TEST 3 *)
(* let tree : int t_avl = avl_lbuild([1;2;3;4;5]);;
let test_seek_6 = avl_seek(tree, 6);;
let test_seek_5 = avl_seek(tree, 5);;
let test_seek_4 = avl_seek(tree, 4);;
let test_seek_3 = avl_seek(tree, 3);;
let test_seek_2 = avl_seek(tree, 2);;
let test_seek_1 = avl_seek(tree, 1);; *)

(* END TESTS SEEK *)



(* TESTS ADD AND DELETE *)

(* TEST 1 *)
(* let tree : int t_avl = avl_lbuild([3;2;1;4;8;6;7]);;
avl_to_string_int(tree);;
let test_avl_delete_7 = avl_delete(tree, 7);;
avl_to_string_int(test_avl_delete_7);;
let test_avl_delete_6 = avl_delete(tree, 6);;
avl_to_string_int(test_avl_delete_6);;
let test_avl_delete_5 = avl_delete(tree, 5);;
avl_to_string_int(test_avl_delete_5);;
let test_avl_delete_4 = avl_delete(tree, 4);;
avl_to_string_int(test_avl_delete_4);;
let test_avl_delete_3 = avl_delete(tree, 3);;
avl_to_string_int(test_avl_delete_3);;
let test_avl_delete_2 = avl_delete(tree, 2);;
avl_to_string_int(test_avl_delete_2);;
let test_avl_delete_1 = avl_delete(tree, 1);;
avl_to_string_int(test_avl_delete_1);; 

let test_avl_delete_max = avl_delete_max(tree);;
avl_to_string_int(test_avl_delete_max);;

let test_add9 = avl_add(tree, 9);;
avl_to_string_int(test_add9);;
let test_add10 = avl_add(test_add9, 10);;
avl_to_string_int(test_add10);; *)

(* TEST 2 *)
(* let tree : int t_avl = avl_lbuild([5;4;3;2;1]);;
avl_to_string_int(tree);;
let test_avl_delete_4 = avl_delete(tree, 4);;
avl_to_string_int(test_avl_delete_4);;
let test_avl_delete_3 = avl_delete(tree, 3);;
avl_to_string_int(test_avl_delete_3);;
let test_avl_delete_2 = avl_delete(tree, 2);;
avl_to_string_int(test_avl_delete_2);;
let test_avl_delete_1 = avl_delete(tree, 1);;
avl_to_string_int(test_avl_delete_1);;

let test_avl_delete_max = avl_delete_max(tree);;
avl_to_string_int(test_avl_delete_max);;

let test_add9 = avl_add(tree, 9);;
avl_to_string_int(test_add9);;
let test_add10 = avl_add(test_add9, 10);;
avl_to_string_int(test_add10);; *)

(* TEST 3 *)
(* let tree : int t_avl = avl_lbuild([1;2;3;4;5]);;
avl_to_string_int(tree);;
let test_avl_delete_4 = avl_delete(tree, 4);;
avl_to_string_int(test_avl_delete_4);;
let test_avl_delete_3 = avl_delete(tree, 3);;
avl_to_string_int(test_avl_delete_3);;
let test_avl_delete_2 = avl_delete(tree, 2);;
avl_to_string_int(test_avl_delete_2);;
let test_avl_delete_1 = avl_delete(tree, 1);;
avl_to_string_int(test_avl_delete_1);;

let test_avl_delete_max = avl_delete_max(tree);;
avl_to_string_int(test_avl_delete_max);;

let test_add9 = avl_add(tree, 9);;
avl_to_string_int(test_add9);;
let test_add10 = avl_add(test_add9, 10);;
avl_to_string_int(test_add10);; *)

(* END TESTS ADD AND DELETE *)





(* TESTS ADD AND DELETE *)

(* TEST 1 *)
(* let tree : int t_avl = avl_lbuild([3;2;1;4;8;6;7]);;
avl_to_string_int(tree);;
let new_tree = avl_convert_imbalance(tree);;
avl_to_string_int_int(new_tree);;
let test_avl_delete_7 = avl_delete2(new_tree, 7);;
avl_to_string_int_int(test_avl_delete_7);;
let test_avl_delete_6 = avl_delete2(new_tree, 6);;
avl_to_string_int_int(test_avl_delete_6);;
let test_avl_delete_5 = avl_delete2(new_tree, 5);;
avl_to_string_int_int(test_avl_delete_5);;
let test_avl_delete_4 = avl_delete2(new_tree, 4);;
avl_to_string_int_int(test_avl_delete_4);;
let test_avl_delete_3 = avl_delete2(new_tree, 3);;
avl_to_string_int_int(test_avl_delete_3);;
let test_avl_delete_2 = avl_delete2(new_tree, 2);;
avl_to_string_int_int(test_avl_delete_2);;
let test_avl_delete_1 = avl_delete2(new_tree, 1);;
avl_to_string_int_int(test_avl_delete_1);; 

let test_avl_delete_max = avl_delete_max2(new_tree);;
avl_to_string_int_int(test_avl_delete_max);;

let test_add9 = avl_add2(new_tree, 9);;
avl_to_string_int_int(test_add9);;
let test_add10 = avl_add2(test_add9, 10);;
avl_to_string_int_int(test_add10);; *)

(* TEST 2 *)
(* let tree : int t_avl = avl_lbuild([5;4;3;2;1]);;
avl_to_string_int(tree);;
let new_tree = avl_convert_imbalance(tree);;
avl_to_string_int_int(new_tree);;
let test_avl_delete_4 = avl_delete2(new_tree, 4);;
avl_to_string_int_int(test_avl_delete_4);;
let test_avl_delete_3 = avl_delete2(new_tree, 3);;
avl_to_string_int_int(test_avl_delete_3);;
let test_avl_delete_2 = avl_delete2(new_tree, 2);;
avl_to_string_int_int(test_avl_delete_2);;
let test_avl_delete_1 = avl_delete2(new_tree, 1);;
avl_to_string_int_int(test_avl_delete_1);;

let test_avl_delete_max = avl_delete_max2(new_tree);;
avl_to_string_int_int(test_avl_delete_max);;

let test_add9 = avl_add2(new_tree, 9);;
avl_to_string_int_int(test_add9);;
let test_add10 = avl_add2(test_add9, 10);;
avl_to_string_int_int(test_add10);; *)

(* TEST 3 *)
(* let tree : int t_avl = avl_lbuild([1;2;3;4;5]);;
avl_to_string_int(tree);;
let new_tree = avl_convert_imbalance(tree);;
avl_to_string_int_int(new_tree);;
let test_avl_delete_4 = avl_delete2(new_tree, 4);;
avl_to_string_int_int(test_avl_delete_4);;
let test_avl_delete_3 = avl_delete2(new_tree, 3);;
avl_to_string_int_int(test_avl_delete_3);;
let test_avl_delete_2 = avl_delete2(new_tree, 2);;
avl_to_string_int_int(test_avl_delete_2);;
let test_avl_delete_1 = avl_delete2(new_tree, 1);;
avl_to_string_int_int(test_avl_delete_1);;

let test_avl_delete_max = avl_delete_max2(new_tree);;
avl_to_string_int_int(test_avl_delete_max);;

let test_add9 = avl_add2(new_tree, 9);;
avl_to_string_int_int(test_add9);;
let test_add10 = avl_add2(test_add9, 10);;
avl_to_string_int_int(test_add10);; *)

(* END TESTS ADD AND DELETE *)
