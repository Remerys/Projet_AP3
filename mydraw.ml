#require "graphics";;
#require "unix";;
#use "ap2inter.ml";;
#use "complex.ml";;
#use "btreeS.ml";;
#use "useBtree.ml";;
#use "bst.ml";;
(*#use "avl.ml";;*)


let sizeX = 800;;
let sizeY = 600;;

open_graph(sizeX,sizeY);;

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
  )
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
let smooth_nb = 4;;
let myind(i : int) = i;;

(*1*)

let draw_height_rnd () =
  let myfunc = height in
  let myparam(i : int) : int t_bst = bst_rnd_create() in

  let (float_index, time_arr) = mycalcul_int(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

let draw_height_increase_rnd() = 
  let myfunc = height in
  let myparam(i : int) : int t_bst = bst_create_rnd_tree(i) in

  let (float_index, time_arr) = mycalcul_int(step_nb, myfunc, myind, myparam, smooth_nb) in
  mydraw(float_index, time_arr, your_float_space_parameter);
;;

(*draw_height_rnd();;*)
(*draw_height_increase_rnd();;*)

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


(*draw_bst_average_imbalance_tree();;*)
(*draw_bst_average_imbalance();;*)

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

(*draw_bst_average_imbalance_subseries();;*)
(*draw_bst_average_imbalance_subseries_random();;*)
(*draw_bst_average_imbalance_subseries_increase();;*)
(*draw_bst_average_imbalance_subseries_decrease();;*)
