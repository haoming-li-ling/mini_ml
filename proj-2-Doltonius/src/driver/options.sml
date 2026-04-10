(* options.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Command-line option processing and compiler flags.
 *)

structure Options : sig

  (* process command-line arguments.  If there is an error, or if the `-h` option
   * is given, then a usage message is printed and the program will exit; otherwise
   * the name of the file to compile is returned.
   *)
    val process : string list -> string

  (* print a usage message to standard output *)
    val usage : unit -> unit

    val optimize : bool ref     (* enable optimization *)

  (* debugging / grading support options *)
    val verbose : bool ref      (* run in verbose mode *)
    val dumpTokens : bool ref   (* dump the token stream *)
    val dumpPT : bool ref       (* dump parse tree *)
    val dumpBT : bool ref       (* dump binding tree *)
    val withMarks : bool ref    (* include source marks when dumping the
                                 * parse/binding trees
                                 *)

  end = struct

    val optimize = ref false
    val verbose = ref false
    val dumpTokens = ref false
    val dumpPT = ref false
    val dumpBT = ref false
    val withMarks = ref false

  (* print a usage message to standard output *)
    fun usage' sts = (
          print
            "usage: mlc [ options ] file.ml\n\
            \    options:\n\
            \      -h             print this message\n\
            \    debug options:\n\
            \      --verbose      verbose mode\n\
            \      --test-scanner dump the token stream to `file.toks`\n\
            \      --dump-pt      dump the parse tree to `file.pt`\n\
            \      --dump-bt      dump the binding tree to `file.bt`\n\
            \      --marks        include source marks in parse/binding-tree output\n\
            \";
          OS.Process.exit sts)

    fun usage () = usage' OS.Process.success

    fun process [file] = if String.isPrefix "-" file
          then usage' OS.Process.failure
          else file
      | process ("--verbose" :: args) = (verbose := true; process args)
      | process ("--test-scanner" :: args) = (dumpTokens := true; process args)
      | process ("--dump-pt" :: args) = (dumpPT := true; process args)
      | process ("--dump-bt" :: args) = (dumpBT := true; process args)
      | process ("--marks" :: args) = (withMarks := true; process args)
      | process ("-h" :: _) = usage()
      | process ("--help" :: _) = usage()
      | process _ = usage' OS.Process.failure

  end
