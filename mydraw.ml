#require "graphics";;
#require "unix";;
#use "ap2inter.ml";;
#use "complex.ml";;
#load "btreeS.cmo";;
#load "useBtree.cmo";;
#load "bst.cmo";;
open BtreeS;;
open UseBtree;;
open Bst;;
(* #use "avl.ml";; *)
#use "avlCouple.ml"



let sizeX = 800;;
let sizeY = 600;;

open_graph(sizeX,sizeY);;
clear_graph();;

let your_float_space_parameter : t_int_space = {
  i_minx = 0 ; 
  i_miny = 0 ; 
  i_maxx = sizeX ; 
  i_maxy = sizeY
};;

let mycalcul_int(step_nb, myfunc, myind, myparam, smooth_nb) : float array * float array = 
  let int_index : int array = arr_make(step_nb + 1, 0)
    and float_index : float array = arr_make(step_nb + 1, 0.0)
    and fun_arr : float array = arr_make(step_nb + 1, 0.0)
    and nb_div_smooth: float = float_of_int(smooth_nb) 
    and var_param : 'a array = arr_make(step_nb + 1, myparam(myind(0)))
  in
    (
    for j = 0 to step_nb
    do
      int_index.(j) <- myind(j) ;
      float_index.(j) <- float_of_int(int_index.(j)) ;
      var_param.(j) <- myparam(int_index.(j)) ;
    done ;
    for i = 1 to smooth_nb
    do
      for j = 0 to step_nb
      do
        fun_arr.(j) <- float_of_int(myfunc(var_param.(j)))
      done ;
    done ; 
    for j = 0 to step_nb
    do fun_arr.(j) <- fun_arr.(j) /. nb_div_smooth
    done ;
    (float_index, fun_arr) ;
    )
;;

let mycalcul(step_nb, myfunc, myind, myparam, smooth_nb) : float array * float array = 
  let int_index : int array = arr_make(step_nb + 1, 0)
    and float_index : float array = arr_make(step_nb + 1, 0.0)
    and fun_arr : float array = arr_make(step_nb + 1, 0.0)
    and nb_div_smooth: float = float_of_int(smooth_nb) 
    and var_param : 'a array = arr_make(step_nb + 1, myparam(myind(0)))
  in
    (
    for j = 0 to step_nb
    do
      int_index.(j) <- myind(j) ;
      float_index.(j) <- float_of_int(int_index.(j)) ;
      var_param.(j) <- myparam(int_index.(j)) ; 
    done ;
    for i = 1 to smooth_nb
    do
      for j = 0 to step_nb
      do
        fun_arr.(j) <- myfunc(var_param.(j))
      done ;
    done ; 
    for j = 0 to step_nb
    do 
      fun_arr.(j) <- fun_arr.(j) /. nb_div_smooth;
    done ;
    (float_index, fun_arr) ;
    )
;;

let mydraw(ind, tm, wd : float array * float array * t_int_space) : unit =
  let indmax : int = arr_len(ind) - 1 in
  let (minind, maxind) : float * float = minmax_float_array(ind) 
    and (mintm, maxtm) : float * float = minmax_float_array(tm) 
  in
  let dind : float = 0.1 *. (maxind -. minind) 
    and dtm : float = 0.1 *. (maxtm -. mintm) 
  in
  let ws : t_float_space = {f_minx = minind -. dind ; f_miny = mintm -. dtm ; 
                   f_maxx = maxind +. 2.0 *. dind ; f_maxy = maxtm +. 2.0 *. dtm}
  in
  let prm : t_float_graphic_parameter = init_float_graphic_parameter(ws, wd) in
  (
    mymoveto_float(0.0 -. dind, 0.0 -. dtm, prm) ; draw_string("(0, 0)") ;
    mymoveto_float(maxind, 0.0 -. dtm, prm) ; draw_string(string_of_float(ind.(indmax))) ;
    mymoveto_float(0.0 -. dind, maxtm, prm) ; draw_string(string_of_float(maxtm)) ;
    mydraw_polyline_xy_float(ind, tm, prm);
    mymoveto_float(maxind, maxtm, prm); draw_string(string_of_float tm.(indmax));

    mymoveto_float ((minind -. dind), 0.0, prm);
    mylineto_float((maxind +. 2.0 *. dind), 0.0, prm);
  )
;;

let create_rnd_list(size : int) : int list=
  let l : int list ref = ref [] in
  for i=0 to size do
    let r : int = Random.int(200) in
    l := r::!l
  done;
  !l
;;

(*
*
*            
*
*                     Calculs
*
*
*
*)

let step_nb = 10;;
let smooth_nb = 3;;
let myind(i : int) = i;;

(*1*)

let draw_height_rnd () =
  let myfunc = bst_height in
  let myparam(i : int) : int t_bst = bst_rnd_create() in

  let (float_index, time_arr) = mycalcul_int(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

let draw_height_increase_rnd() = 
  let myfunc = bst_height in
  let myparam(i : int) : int t_bst = bst_create_rnd_tree(i) in

  let (float_index, time_arr) = mycalcul_int(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

(*2*)

let draw_bst_average_imbalance_tree () =
  let myfunc = bst_average_imbalance_tree in
  let myparam(i : int) : int t_bst = bst_create_rnd_tree(i) in

  let (float_index, time_arr) = mycalcul(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

let draw_bst_average_imbalance() =
  let myfunc = bst_average_imbalance_aux in
  let myparam(i : int) : unit = () in

  let (float_index, time_arr) = mycalcul(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

(*3*)

let draw_bst_average_imbalance_subseries () =
  let myfunc = bst_imbalance_subseries in
  let myparam(i : int) : int = 4 in

  let (float_index, time_arr) = mycalcul(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

let draw_bst_average_imbalance_subseries_random () =
  let myfunc = bst_imbalance_subseries in
  let r : int = Random.int(10)+1 in 
  let myparam(i : int) : int = r in

  let (float_index, time_arr) = mycalcul(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

let draw_bst_average_imbalance_subseries_increase () =
  let myfunc = bst_imbalance_subseries in
  let myparam(i : int) : int = i+1 in

  let (float_index, time_arr) = mycalcul(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

let draw_bst_average_imbalance_subseries_decrease () =
  let myfunc = bst_imbalance_subseries in
  let myparam(i : int) : int = step_nb-i+1 in

  let (float_index, time_arr) = mycalcul(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

(* TESTS COMPLEXITY OF AVL_SEEK | AVL_ADD | AVL_DELETE | AVL_DELETE_MAX*)

let complexity_AVL_REBALANCE() =
  let step_nb = 750 in
  let myfunc = avl_rebalance2 in
  let myind (i : int) : int = i in
  let myparam = (fun i -> avl_convert_imbalance(avl_lbuild(create_rnd_list(i)))) in
  let smooth_nb = 10000 in

  let result = mycomplexity(step_nb, myfunc, myind, myparam, smooth_nb) in
  match result with
  | (float_index, time_arr) ->
    mydraw_complexity(float_index, time_arr, your_float_space_parameter);
;;

let complexity_AVL_DELETE_MAX() =
  let step_nb = 50 in
  let myfunc = avl_delete_max2 in
  let myind (i : int) : int = i in
  let myparam = (fun i -> avl_convert_imbalance(avl_lbuild(create_rnd_list(i)))) in
  let smooth_nb = 1000 in

  let result = mycomplexity(step_nb, myfunc, myind, myparam, smooth_nb) in
  match result with
  | (float_index, time_arr) ->
    mydraw_complexity(float_index, time_arr, your_float_space_parameter);
;;

let complexity_AVL_DELETE() =
  let step_nb = 50 in
  let myfunc = avl_delete2 in
  let myind (i : int) : int = i in
  let myparam = (fun i -> (avl_convert_imbalance(avl_lbuild(create_rnd_list(i)))), i) in
  let smooth_nb = 1000 in

  let result = mycomplexity(step_nb, myfunc, myind, myparam, smooth_nb) in
  match result with
  | (float_index, time_arr) ->
    mydraw_complexity(float_index, time_arr, your_float_space_parameter);
;;

let complexity_AVL_ADD() =
let step_nb = 50 in
let myfunc = avl_add2 in
let myind (i : int) : int = i in
let myparam = (fun i -> (avl_convert_imbalance(avl_lbuild(create_rnd_list(i)))), i) in
let smooth_nb = 1000 in

let result = mycomplexity(step_nb, myfunc, myind, myparam, smooth_nb) in
match result with
| (float_index, time_arr) ->
  mydraw_complexity(float_index, time_arr, your_float_space_parameter);
;;

let complexity_AVL_SEEK() =
let step_nb = 50 in
let myfunc = avl_seek in
let myind (i : int) : int = i in
let myparam = (fun i -> avl_lbuild(create_rnd_list(i)), i) in
let smooth_nb = 1000 in

let result = mycomplexity(step_nb, myfunc, myind, myparam, smooth_nb) in
match result with
| (float_index, time_arr) ->
  mydraw_complexity(float_index, time_arr, your_float_space_parameter);
;;


let func(i : int) : int =
  reset_counter();
  let t = avl_lbuild(subseries(100, 100-i/10, 200)) in
  get_counter()
;;

let avl_NB_ROTATE() =
  let step_nb = 1000 in
  let myfunc = func in
  let myind (i : int) : int = i in
  let myparam = (fun i -> i) in
  let smooth_nb = 1 in

  let result = mycalcul_int(step_nb, myfunc, myind, myparam, smooth_nb) in
  match result with
  | (float_index, time_arr) ->
    mydraw(float_index, time_arr, your_float_space_parameter);
;; 

