/* goodbye-world.ml */

let hello = "Hello";
let goodbye = "Goodbye";
let world = "world";

let _ = print (hello ^ ", " ^ world ^ "!\n");

fun f n = {
    fun loop i =
	  if i < n then {print " cruel"; loop (i + 1)}
	  else ();
    print (goodbye ^ ",");
    loop 0;
    print (" " ^ world ^ "!\n")
  };

f 5
