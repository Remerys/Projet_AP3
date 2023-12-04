(* ----------------------------------- *)
(*  calcul de complexite de fonctions  *)
(* mycomplexity : calcul des tableaux  *)
(* mydraw_complexity : trace de courbe *)
(* ----------------------------------- *)

let time_eval(myfunc, myparam : ('a -> 'b) * 'a) : float = 
  let mem_tm : float = Sys.time() in
    (
    ignore(myfunc(myparam)) ;
    Sys.time() -. mem_tm
    )
;;
let smooth_time_eval(myfunc, myparam, smooth_nb : ('a -> 'b) * 'a * int) : float = 
  let mem_tm : float ref = ref 0.0 and tm_sum : float ref = ref 0.0 in
    (
    for i = 1 to smooth_nb
    do
      mem_tm := Sys.time() ;
      ignore(myfunc(myparam)) ;
      tm_sum := !tm_sum +. (Sys.time() -. !mem_tm)
    done ;
    !tm_sum /. float_of_int(smooth_nb)
    )
;;
let mycomplexity(step_nb, myfunc, myind, myparam, smooth_nb : int * ('a -> 'b) * (int -> int) * (int -> 'a) * int) : float array * float array = 
  let int_index : int array = arr_make(step_nb + 1, 0)
    and float_index : float array = arr_make(step_nb + 1, 0.0)
    and time_arr : float array = arr_make(step_nb + 1, 0.0)
    and nb_div_smooth: float = float_of_int(smooth_nb) 
    and var_param : 'a array = arr_make(step_nb + 1, myparam(myind(0)))
    and memtime : float ref = ref 0.0
  in
    (
    for j = 0 to step_nb
    do
      int_index.(j) <- myind(j) ;
      float_index.(j) <- float_of_int(int_index.(j)) ;
      (*var_param.(j) <- myparam(int_index.(j)) ;*)
    done ;
    for i = 1 to smooth_nb
    do
      for j = 0 to step_nb
      do
        var_param.(j) <- myparam(int_index.(j)); 
        memtime := Sys.time() ;
        ignore(myfunc(var_param.(j))) ; 
        time_arr.(j) <- time_arr.(j) +. (Sys.time() -. !memtime) ;
      done ;
    done ; 
    for j = 0 to step_nb
    do time_arr.(j) <- time_arr.(j) /. nb_div_smooth
    done ;
    (float_index, time_arr) ;
    )
;;
let minmax_float_array(t : float array) : float * float =
  let indmax : int = arr_len(t) - 1 in
  let min : float ref = ref 0.0 and max : float ref = ref 0.0 in
    if indmax < 0
    then failwith("erreur minmaxfloatarray : tableau vide") 
    else
      (
      min := t.(0) ; max := t.(0) ;
      for i = 1 to indmax 
      do 
        if !min <= t.(i) then () else min := t.(i) ;
        if !max >= t.(i) then () else max := t.(i)
      done ;
      (!min, !max)
      )
;;
let mydraw_complexity(ind, tm, wd : float array * float array * t_int_space) : unit =
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
    mydraw_polyline_xy_float(ind, tm, prm)
    )
;;