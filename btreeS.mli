(* type abstrait*)
type 'a btreeS

(* creer un arbre vide*)
val btreeS_empty :  unit -> 'a btreeS

(* creer un arbre a partir de sa racine et de ses deux sous arbres*)
val btreeS_rooting : 'a * 'a btreeS * 'a btreeS -> 'a btreeS

(* Verifie s'il est vide*)
val btreeS_isempty : 'a btreeS -> bool

(* Retourne la racine de l'arbre*)
val btreeS_root : 'a btreeS -> 'a

(*Retourne l'arbre gauche de l'arbre*)
val btreeS_subleft : 'a btreeS -> 'a btreeS

(*Retourne l'arbre droit de l'abre*)
val btreeS_subright : 'a btreeS -> 'a btreeS
