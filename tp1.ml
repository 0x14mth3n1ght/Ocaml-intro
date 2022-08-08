let f = fun x -> atan x;;

let pi = (f 1.)*.4.;;
print_string "pi=";;
print_float(pi);;
print_newline();;

let sphere_vol = fun r -> (4./.3.)*.pi*.(r**3.);;
let r = (sphere_vol 1.);;
print_string "vol sphère de rayon 1=";;
print_float(r);;
print_newline();;

(*let mod1 = fun n -> if n mod 2=0 then true else false;; pas bien -> on va renvoyer directement la valeur du test*)
let mod1 = fun n -> n mod 2=0 ;;
Printf.printf "%B\n" (mod1 2);;

let mod2 = fun n -> if n mod 2=0 then print_string "Pair\n" else print_string "Impair\n";;
(mod2 47);;

let mod3 = fun n -> if n mod 2=0 then Printf.printf "le nombre %i est pair \n" n else Printf.printf "le nombre %i est impair \n" n;;
(mod3 58);;

(*let test_rectangle  c1 c2 h = if c1**2. +. c2**2. = h**2. then print_string "rectangle\n" else print_string "pas rectangle";;
(test_rectangle 1. 2. 5.);;*)
let test_rectangle  c1 c2 h = if c1*c1 + c2*c2 = h*h then print_string "rectangle\n" else print_string "pas rectangle\n";;
(test_rectangle 1 2 5);;