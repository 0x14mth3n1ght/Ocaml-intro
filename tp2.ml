(*Partie 1*)
type rat ={num: int;den: int};;

let add=fun rat1 rat2 -> {num=rat1.num*rat2.den+rat2.num*rat1.den;den=rat1.den*rat2.den};;

let rat1={num=1;den=4};;
let rat2={num=3;den=4};;
let rat3=(add rat1 rat2);;
print_string "num=";;
print_int(rat3.num);;
print_newline();;
print_string "den=";;
print_int(rat3.den);;

let add_couple (p1,q1) (p2,q2) = (p1*q2 + p2*q1, q1*q2);;
let r1=(1,4);;
let r2=(1,2);;
let rat_couple3=(add_couple r1 r2);;
print_newline();;
print_string "num=";;
print_int(fst(rat_couple3));;
print_newline();;
print_string "den=";;
print_int(snd(rat_couple3));;
print_newline();;

let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b);;

  let add_couple_reduit (p1,q1) (p2,q2) = 
    ((p1*q2 + p2*q1)/(gcd (p1*q2 + p2*q1) (q1*q2)), (q1*q2)/(gcd (p1*q2 + p2*q1) (q1*q2)));;

(*Partie2*)

type number=I of int | R of float;;
let pi=R (4.0*.atan 1.0);;
let one=I 1;;

let add_one=fun nb-> match nb with
  |I i -> I (i+1)
  |R b -> R (b+.1.0);;