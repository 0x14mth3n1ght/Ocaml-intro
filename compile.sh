for i in *ml 
do 
ocamlc -o  ${i%.*} "$i"
done
