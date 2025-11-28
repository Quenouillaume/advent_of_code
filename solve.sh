

ocamlc unix.cma util.ml $0/day$1.ml
./a.out > out.txt
submit.py $0 $1 out.txt