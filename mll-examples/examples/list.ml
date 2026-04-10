/* list.ml */

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

fun foldl f b l =
  case l of
    { Nil => b }
    { hd :: tl => foldl f (f hd b) tl }
  end;

fun rev xs = {
      fun cons hd tl = hd :: tl;
      foldl cons Nil xs
  };

fun tabulate (n, f) = {
      fun loop (i, acc) =
         if i <= n
           then loop (i + 1, (f i) :: acc)
           else rev acc;
     if n < 0
       then fail "tabulate: n < 0"
       else loop (0, Nil)
  };

let n = 4999;
let sum_n = {
    fun f x y = x + y;
    fun g i = i;
    foldl f 0 (tabulate (n, g))
  };

print "foldl \n";
print "      (fn x y => x + y)\n";
print "      0 (tabulate (";
print (intToString n);
print ", (fn i => i))) = ";
print (intToString sum_n);
print "\n";
()
