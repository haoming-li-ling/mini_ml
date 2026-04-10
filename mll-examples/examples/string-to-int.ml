/* string-to-int.ml */

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

data Option [a] = None | Some of a;

fun optionToString aToString opt =
      case opt of
	{ None => "None" }
	{ Some x => "Some(" ^ aToString x ^ ")" }
      end;

let ord0 = sub ("0", 0);
let ord9 = sub ("9", 0);

fun charToDigit c =
      if ord0 <= c
	then if c <= ord9
	  then Some(c - ord0)
	  else None
	else None;

fun stringToInt s = {
      let sz = size s;
      fun loop (pos, acc)=
	if pos < sz
	  then case charToDigit (sub (s, pos)) of
		{ None => None }
		{ Some i => loop (pos + 1, acc * 10 + i) }
	       end
	else Some acc;
      if 0 < sz then loop (0, 0)
      else None
    };

case arguments of
 { s :: _ =>
     print (optionToString intToString (stringToInt s));
     print "\n"
   }
 { Nil => () }
end
