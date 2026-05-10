for NAME in args divide-by-zero even-odd fact-y fib-y fib goodbye-world hello-world id int-to-string list okay one-plus-one simple-fact simple-fib string-to-int
do
	~/cmsc226/proj-4-Doltonius/bin/mlc.sh --dump-cfg ~/cmsc226/mll-examples/examples/$NAME.ml
	diff ~/cmsc226/mll-examples/examples/$NAME.cfg ~/cmsc226/mll-examples/cfg/$NAME.cfg > ~/cmsc226/mll-examples/examples/diffcfg/$NAME.txt
done
