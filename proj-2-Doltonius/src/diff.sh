for NAME in args divide-by-zero even-odd fact-y fib-y fib goodbye-world hello-world id int-to-string list okay one-plus-one simple-fact simple-fib string-to-int
do
	~/cmsc226/proj-2-Doltonius/bin/mlc.sh --dump-pt ~/cmsc226/mll-examples/examples/$NAME.ml
	diff ~/cmsc226/mll-examples/examples/$NAME.pt ~/cmsc226/mll-examples/parse-trees/$NAME.pt
done