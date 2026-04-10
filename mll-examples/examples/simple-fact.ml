/* simple-fact.ml */

let zero = 0;
let one = 1;
fun fact n = if n <= zero then one else n * fact (n - one);

if fact 5 == 120 then print "Ok\n" else print "Fail\n"
