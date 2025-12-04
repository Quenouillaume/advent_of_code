# Compute total time for solving AOC 

echo > out_time.txt
for (( i = 1; i <= 4; i++ )); do
	ocamlc unix.cma advent_of_code.ml 2025/day_$i.ml -w -A
	(time ./a.out) 2>> out_time.txt
done

cat out_time.txt | grep real | awk '{print $2}'
rm a.out out_time.txt
