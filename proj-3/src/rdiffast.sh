for NAME in args2 divide-by-zero even-odd fact-y fib-y fib goodbye-world hello-world id int-to-string list2 okay one-plus-one2 simple-fact simple-fib string-to-int2
do
	~/cmsc226/proj-3-Doltonius/bin/mlc.sh --dump-ast ~/cmsc226/mll-examples/examples/$NAME.ml
	# diff ~/cmsc226/mll-examples/examples/$NAME.ast ~/cmsc226/mll-examples/bind-trees/$NAME.bt > ~/cmsc226/mll-examples/examples/diff/$NAME.txt
done
