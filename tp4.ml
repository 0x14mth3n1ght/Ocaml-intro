let aux (nbTrueMax,nbTrueCur) b=
  if b then let nbTrueCur'=nbTrueCur+1 in (max nbTrueMax nbTrueCur', nbTrueCur')
  else (nbTrueMax,0)

let max_seq b l =
  fst(List.fold_left aux (0,0) l);;

let () = assert (max_seq [] = 0)
let () = assert (max_seq [false; false; false] = 0)
let () = assert (max_seq [true; false; true; true; true; false; false] = 3) 