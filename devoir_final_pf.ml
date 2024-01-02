(* Amazigh ALLOUN 12007813
   Dieunel MARCELIN 12207041 *)
   
type term =
  | Var of string
  | Func of string * term list
  | Const of string

type subst = (string * term) list
exception FAILURE

let l = ref [];;
let cpt = ref 0 ;;

let rec occurs_check x t =
  match t with
  | Var y -> x = y
  | Func (_, args) -> List.exists (occurs_check x) args
  | _ -> false

let rec unif t1 t2 =
  match (t1, t2) with
  | Const c1, Const c2 ->
      if c1 = c2 then [] else raise FAILURE
  | Var x, _ ->
      if t1 = t2 then [] else if occurs_check x t2 then raise FAILURE else [(x, t2)]
  | _, Var x ->
      if t1 = t2 then [] else if occurs_check x t1 then raise FAILURE else [(x, t1)]
  | Func (f1, args1), Func (f2, args2) ->
      if f1 = f2 then unify_args args1 args2
      else raise FAILURE
  | _ -> raise FAILURE


and unify_args args1 args2 =
  match (args1, args2) with
  | [], [] -> []
  | [], _ | _, [] -> raise FAILURE
  | t1 :: q1, t2 :: q2 ->
      let head_substitution = unif t1 t2 in
      let tail_substitution =
        unify_args
          (List.map (fun t -> substitute_term t head_substitution) q1)
          (List.map (fun t -> substitute_term t head_substitution) q2) in
      		tail_substitution@ head_substitution 


and substitute_term term sub =
  match term with
  | Var x ->
      (try List.assoc x sub with Not_found -> term) 
  | Func (f, args) ->
      Func (f, List.map (fun t -> substitute_term t sub) args) 
  | _ -> term
  
  
let rec anti_unif t1 t2 =
  match t1, t2 with
  | Func (f, args1), Func (g, args2) when f <> g ->
        raise FAILURE
  | Func (_, args1), Func (_, args2) when List.length args1 <> List.length args2 ->
        raise FAILURE
  | Func (f1, args1), Func (f2, args2) when f1 = f2 ->
      let anti_unif_args = List.map2 anti_unif args1 args2 in
      Func (f1, anti_unif_args)
  | _ ->
      match List.assoc_opt (t1, t2) !l with
      | Some j -> Var ("Z" ^ string_of_int j)
      | None ->
          let i = !cpt in
          cpt := !cpt + 1;
          let zi = Var ("Z" ^ string_of_int i) in
          l := ((t1, t2), i) :: !l;
          zi
 ;;

(*
Quelques exemples à tester
: (sous environnement OCaml ou utop
Sous lin ux , o uv re z un terminal dans le répertoire du fichier « de voir_pf_final.ml » , tapez
ocaml ou utop au clavier et enfin la commande #use devoir_pf_final.ml
u nif
1) unif (Var "x") (Const "d");; ((* affiche (string * term) list = [("x", Const "
2) unif (Var "x") (Var "x");; (* af f iche (string * term) list =
3) unif (Func ("f", [Var "x"])) (Func ("g", [Var "z"]));; aff iche ex ception FAILURE
4) unif (Func ("f", [Var "a"; Var "y"])) (Func ("f", [Const "w"; Var "z"]));;
(*
affiche (string * term) list = [("y", Var "z"); ("a", Const "
5) unif (Func ("f", [Var "x"; Var "y"])) (Func ("f", [Var "z"; Func("g", [Var"k"])]));;
(*
affiche (string * term) list = [("y", Func ("g", [Var "k"])); ("x", Var "
6) unif (Func ("f", [Var "a"; Var "y"; Var"v"])) (Func ("f", [Const "w"; V ar "z"; Const"b"]));;
(*
affiche (string * term) list = [("v", Const "b"); ("y", Var "z"); ("a", Const "
a
nti_unif : (recomp ilez le programme
1)
anti_unif (Var "x") (Const "d");; aff iche term = Var "Z0"
2)
anti_ unif (Var "x") (Var "x");; affiche term = Var "Z1"
4
3
3) ) anti_unif (Func ("f", [Var "x"])) (Func ("g", [Var"a"; Var "z"]))anti_unif (Func ("f", [Var "x"])) (Func ("g", [Var"a"; Var "z"]));;;; (*(*affiche affiche Exception:Exception:FAILUREFAILURE*)*)
4)
4) anti_unif (Func ("f", [Var "a"; Var "y"; Var"v"])) (Func ("f", [Const "w"; Var "z"; Const"b"]));;anti_unif (Func ("f", [Var "a"; Var "y"; Var"v"])) (Func ("f", [Const "w"; Var "z"; Const"b"]));;
(*(* affaffiche iche term = Func ("f", [Var "Z3"; Var "Z4"; Var "Z5"])term = Func ("f", [Var "Z3"; Var "Z4"; Var "Z5"])*)*)
5)
5) anti_unif (Func ("f", [Var "a"; Var "y"; Var"v"])) (Func ("f", [Var "w"; Var "z"]));;anti_unif (Func ("f", [Var "a"; Var "y"; Var"v"])) (Func ("f", [Var "w"; Var "z"]));;
(*
(* afficheaffiche exexceptionception :: FAILURE *)FAILURE *)
*)

