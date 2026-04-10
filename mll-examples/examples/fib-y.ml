/* fib-y.ml */

fun intToString i =
   if i < 0 then "-" ^ (intToString (- i))
   else if i == 0 then "0"
   else if i == 1 then "1"
   else if i == 2 then "2"
   else if i == 3 then "3"
   else if i == 4 then "4"
   else if i == 5 then "5"
   else if i == 6 then "6"
   else if i == 7 then "7"
   else if i == 8 then "8"
   else if i == 9 then "9"
   else (intToString (i / 10)) ^ (intToString (i % 10));

/* the Y combinator */
fun y f = {
    fun g z = y f ();
    f g
  };

fun fibY fib z n =
   if n <= 1 then 1 else (fib z) (n - 1) + (fib z) (n - 2);

let fib = y fibY ();

let n = 20;
let fib_n = fib n;

print "fib ";
print (intToString n);
print " = ";
print (intToString fib_n);
print "\n";
()