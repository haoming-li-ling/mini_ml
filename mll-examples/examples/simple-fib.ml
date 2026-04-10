/* fib.ml */

fun fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);

if fib 5 == 8 then print "Ok\n" else print "Fail\n"
