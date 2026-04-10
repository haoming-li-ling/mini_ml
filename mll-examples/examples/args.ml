/* args.ml */

fun fst p = case p of { (a, _) => a } end;
fun snd p = case p of { (_, b) => b } end;

fun length l = case l of
      { Nil => 0 }
      { _ :: r => 1 + length r }
      end;

fun appi f l = {
      fun appf arg = case snd arg of
            { Nil => () }
            { x :: xs => let i = fst arg; f (i, x); appf (i+1, xs) }
            end;
      appf (0, l)
    };

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

fun printArg arg = {
      print "arg ";
      print (intToString (fst arg));
      print " = ";
      print (snd arg);
      print "\n"};

let nArgs = length arguments;
let _ = print ("# args = " ^ (intToString nArgs) ^ "\n");
appi printArg arguments
