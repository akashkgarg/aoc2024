day01:
	clj -X day01/run :data \"01-full.txt\"
day02:
	clj -X day02/run :data \"02-full.txt\"

day14small:
	clj -X day14/run :data \"14-small.txt\" :width 11 :height 7 :steps 100

day14full:
	clj -X day14/run :data \"14-full.txt\" :width 101 :height 103 :steps 100
