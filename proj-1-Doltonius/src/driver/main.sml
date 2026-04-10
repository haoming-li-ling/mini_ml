(* main.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Driver for ML-Lite compiler.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure Opts = Options

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm =
          if Error.anyErrors errStrm
            then raise Error.ERROR
            else ()

  (* verbose printing *)
    val verbose = Opts.verbose
    fun say msg = if !verbose then print(concat("# " :: msg)) else ()

    fun handleExn Error.ERROR = OS.Process.failure
      | handleExn exn = (
          err (concat [
              "uncaught exception ", General.exnName exn,
              " [", General.exnMessage exn, "]\n"
            ]);
          List.app
            (fn s => err (concat ["  raised at ", s, "\n"]))
              (SMLofNJ.exnHistory exn);
          OS.Process.failure)

  (* make an error stream for an input file and limit the number of reported errors
   * to 25.
   *)
    fun mkErrStream srcFile = let
	  val errStrm = Error.mkErrStream srcFile
	  in
	    Error.setErrorLimit (errStrm, 25);
	    errStrm
	  end

    fun main (_, opts) = let
          val file = Options.process opts
          in
          (* check that the input file exists *)
            if not (OS.FileSys.access(file, [OS.FileSys.A_READ]))
              then (
                err (concat[
                    "source file \"", file,
                    "\" does not exist or is not readable\n"
                  ]);
                OS.Process.failure)
          (* process the file *)
            else let (* scanner test *)
              (* scan the input and print the tokens to <file>.toks *)
                val errStrm = mkErrStream file
                val base = OS.Path.base file
                val outFile = OS.Path.joinBaseExt{base = base, ext = SOME "toks"}
                val inS = TextIO.openIn file
                val outS = TextIO.openOut outFile
                val _ = say ["testing scanner; output to ", outFile, "\n"]
                val sts = Parser.lexer (inS, errStrm, outS)
                in
                  TextIO.closeIn inS; TextIO.closeOut outS;
                  if sts then OS.Process.failure else OS.Process.success
                end
          end (* main *)
            handle ex => handleExn ex

  end
