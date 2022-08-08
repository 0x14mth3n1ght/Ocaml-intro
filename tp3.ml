(* II *)
(* Cons = constructeur cf complexes*)

type iliste = Nil | Cons of (int*iliste);;

let ajoute n l = Cons(n,l);;

(*let recherche n l = match n with
    | []->false
    | *)

let rec fold_left f l acc = match l with
  | Nil -> acc
  | Cons(x,xs) -> fold _left f (f acc x) xs;;  
  
let recherche n l = fold_left (fun acc x-> acc || x=n) false l;;