(* Question 1 *)
let rec nbbase b n =
  if n < b then 1
  else 1 + nbbase b (n / b);;

let b = 2;;
let n = 15;;
Format.printf "Q1. b: %d, n: %d => nbbase: %d\n" b n (nbbase b n);;

(* LISTES D'OBJETS DE TYPE INT *)

(* Question 2 *)
type iliste =
  | IEmpty
  | ICons of (int * iliste);;

(* Question 3 *)
let ajoute n l = ICons (n , l);;

(* Question 4 *)
let rec recherche n l =
  match l with
  | IEmpty -> false
  | ICons (n', l') ->
      if n' = n then true
      else recherche n l';;

(* Question 5 *)
let rec il_existe_pair l =
  match l with
  | IEmpty -> false
  | ICons (n, l') ->
      if n mod 2 = 0 then true
      else il_existe_pair l';;

(* Question 6 *)
let rec supprime n l =
  match l with
  | IEmpty -> IEmpty
  | ICons (n', l') ->
      if n' = n then l'
      else ICons (n', supprime n l');;

(* Question 7 *)
let rec supprime_tout n l =
  match l with
  | IEmpty -> IEmpty
  | ICons (n', l') ->
      if n' = n then supprime_tout n l'
      else ICons (n', supprime_tout n l');;

(* Tests questions 2 à 7 *)

(* Non demandée, pratique *)
let rec string_of_iliste l =
  match l with
  | IEmpty -> "[]"
  | ICons (a, l') -> string_of_int a ^ "::" ^ string_of_iliste l';;

let v = IEmpty;;
let v = ajoute 2 v;;
let v = ajoute 4 v;;
let n = 4;;
Format.printf "Q2/3/4. recherche de %d dans %s: %b\n" n (string_of_iliste v) (recherche n v);;
Format.printf "Q5. nombre pair dans %s: %b\n" (string_of_iliste v) (il_existe_pair v);;
Format.printf "Q6. %s - suppression de %d - %s\n" (string_of_iliste v) n (string_of_iliste (supprime n v));;
let v = ajoute 5 v;;
let v = ajoute 5 v;;
let n = 5;;
Format.printf "Q7. %s - suppression de tous les %d - %s\n" (string_of_iliste v) n (string_of_iliste (supprime_tout n v));;

(* LISTES POLYMORPHES *)

(* Définition du type *)
type 'a liste =
  | LEmpty
  | LCons of ('a * 'a liste);;

(* Non demandée mais très très pratique *)
let rec foldLeft f v0 l =
  match l with
  | LEmpty -> v0
  | LCons (a, l') ->
      let v1 = f v0 a in
      foldLeft f v1 l';;

(* Question 8 *)
let ajoute2 n l = LCons (n , l);;

(* Question 9 *)
let inverse l =
  match l with
  | LEmpty -> LEmpty
  | LCons (_, l') -> foldLeft (fun li n -> LCons (n, li)) LEmpty l;;

(* Question 10 *)
let rec il_existe p l =
  match l with
  | LEmpty -> false
  | LCons (a, l') ->
      if p a then true
      else il_existe p l';;

(* Tests questions 8 à 10 *)
let vi = LEmpty;;
let vi = ajoute2 2 vi;;
let vi = ajoute2 4 vi;;
let vi = ajoute2 4 vi;;

let vf = LEmpty;;
let vf = ajoute2 2.0 vf;;
let vf = ajoute2 4.0 vf;;
let vf = ajoute2 4.0 vf;;

let vi = inverse vi;;
let vf = inverse vf;;

Format.printf "Q8/9/10. il_existe n > 4 dans vi: %b\n" (il_existe (fun n -> n > 4) vi);;
Format.printf "         il_existe n > 3.0 dans vf: %b\n" (il_existe (fun n -> n > 3.0) vf);;

(* MANIPULATION DE LISTES *)

(* Listes d'associations *)

(* Question 11 *)
let rec recherche_k k l =
  match l with
  | [] -> raise Not_found
  | (k', v)::l' ->
      if k = k' then v
      else recherche_k k l';;

(* Question 12 *)
let rec recherche_k' k l =
  match l with
  | [] -> []
  | (k', v)::l' ->
      let res = recherche_k' k l' in
      if k = k' then v::res
      else res;;

(* Tests questions 11 et 12 *)

(* Non demandée, pratique *)
let rec string_of_i_s_list l =
  match l with
  | [] -> "[]"
  | (a, s)::l' -> "(" ^ string_of_int a ^ ", " ^ s ^ ")::" ^ string_of_i_s_list l';;

(* Non demandée, pratique *)
let rec string_of_s_list l =
  match l with
  | [] -> "[]"
  | s::l' -> s ^ "::" ^ string_of_s_list l';;

let vk = (1, "a")::(1, "b")::(2, "a")::[];;
Format.printf "Q12. k=1, premier elem de clé 1 dans %s: %s\n" (string_of_i_s_list vk) (recherche_k 1 vk);; 
Format.printf "Q12. k=1, tout les elems de clé 1 dans %s: %s\n" (string_of_i_s_list vk) (string_of_s_list (recherche_k' 1 vk));; 

(* Minimum et maximum d'une liste *)

(* Question 13 *)
let rec maximum l =
  match l with
  | [] -> min_int
  | v::l' -> max v (maximum l');;

(* Question 14 *)
let maximum' l = List.fold_left (fun a b -> max a b) min_int l;;

(* Question 15 *)
let min_max l = 
  (List.fold_left (fun a b -> min a b) max_int l,
   List.fold_left (fun a b -> max a b) min_int l);;

(* Tests questions 13 à 15 *)
let rec string_of_i_list l =
  match l with
  | [] -> "[]"
  | i::l' -> string_of_int i ^ "::" ^ string_of_i_list l';;

(* Non demandée, pratique *)
let string_of_couple c =
  match c with
  | (a, b) -> "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")";;

let vI = [1;2;3;4];;
Format.printf "Q13. max dans %s: %d\n" (string_of_i_list vI) (maximum vI);;
Format.printf "Q14. max dans %s: %d\n" (string_of_i_list vI) (maximum' vI);;
Format.printf "Q15. min-max dans %s: %s\n" (string_of_i_list vI) (string_of_couple (min_max vI));;

(* Minimum et maximum d'une liste, avec moins de comparaisons *)

(* Question 16 *)
let rec grouper l =
  match l with
  | [] -> []
  | a::[] -> (a, a)::[]
  | a::b::l' -> (a, b)::(grouper l');;

(* Question 17 *)
let rec trier_couple l =
  match l with
  | [] -> []
  | (a, b)::l' -> (min a b, max a b)::(trier_couple l');;

(* Question 18 *)
let min_max' l =
  let v = trier_couple (grouper l) in
  List.fold_left (fun (a, b) (c, d) -> (min a c, max b d)) (max_int, min_int) v;;

(* Tests des question 16 à 18 *)

let vI = [5; 12; 7; 9];;
Format.printf "Q16/17/18. min-max dans %s: %s\n" (string_of_i_list vI) (string_of_couple (min_max' vI));;
