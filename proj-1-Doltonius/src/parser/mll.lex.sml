structure MLLLex  = struct

    datatype yystart_state = 
BLOCKCOMMENT | INQUOTE | INITIAL | LINECOMMENT
    local

    structure UserDeclarations = 
      struct



    structure T = MLLTokens

    type lex_result = T.token

    (* this implements a buffer to store strings scanned *)
    val buf = ref ""

    (* creates a new buffer *)
    fun new () = (buf := "")

    (* append a string to the buffer *)
    fun add s = (buf := (!buf) ^ s)

    (* get the content of the buffer *)
    fun get () = !buf


    (* this implements a counter for the number of open block comments *)
    val commentCount = ref 0

    (* creates a new comment counter *)
    fun newComment () = (commentCount := 0)

    (* adds one to the comment counter *)
    fun addComment () = (commentCount := (!commentCount) + 1)

    (* subtracts one from the comment counter *)
    fun minusComment () = (commentCount := (!commentCount) - 1)

    (* checks whether the comment counter is zero *)
    fun isZeroComment () = (!commentCount) = 0


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    val yytable : ((UTF8.wchar * UTF8.wchar * int) list * int list) Vector.vector = 
Vector.fromList []
    fun yystreamify' p input = ULexBuffer.mkStream (p, input)

    fun yystreamifyReader' p readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            yystreamify' p input
          end

    fun yystreamifyInstream' p strm = yystreamify' p (fn ()=>TextIO.input strm)

    fun innerLex 
(yyarg as  lexErr)(yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yysetStrm strm = yystrm := strm
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
	  fun yystreamify input = yystreamify' (yygetPos()) input
	  fun yystreamifyReader readFn strm = yystreamifyReader' (yygetPos()) readFn strm
	  fun yystreamifyInstream strm = yystreamifyInstream' (yygetPos()) strm
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case ULexBuffer.getu strm
                of (SOME (0w10, s')) => 
		     (AntlrStreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = AntlrStreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = AntlrStreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    val yylastwasnref = ref (ULexBuffer.lastWasNL (!yystrm))
	    fun continue() = let val yylastwasn = !yylastwasnref in
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;  skip ())
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_case)
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_data)
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_else)
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_end)
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_fun)
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_if)
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_let)
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_of)
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;  T.KW_then)
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LP)
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RP)
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LB)
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RB)
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LCB)
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;  T.RCB)
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ASSIGN)
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ORELSE)
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ANDALSO)
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;  T.EQEQ)
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;  T.NEQ)
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LTEQ)
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;  T.LT)
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;  T.CONS)
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;  T.CONCAT)
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;  T.PLUS)
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;  T.MINUS)
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;  T.TIMES)
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DIV)
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;  T.MOD)
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DEREF)
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;  T.EQ)
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;  T.COMMA)
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;  T.SEMI)
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;  T.BAR)
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;  T.ARROW)
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;  T.DARROW)
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;  T.WILD)
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.UID (Atom.atom yytext)
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.LID (Atom.atom yytext)
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  T.NUMBER (valOf (IntInf.fromString yytext))
      end
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INQUOTE; new (); continue ())
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; T.STRING (get ()))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
       lexErr ((yypos, yypos), 
                                     ["illegal whitespace in string"]); 
                             continue ())
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
       lexErr ((yypos, yypos), 
                                     ["illegal escape code \\000"]); 
                             continue ())
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         add (valOf (String.fromString yytext)); 
                             continue ()
      end
fun yyAction46 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  add yytext; continue ()
      end
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN LINECOMMENT; skip ())
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN INITIAL; skip ())
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;  skip ())
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
       YYBEGIN BLOCKCOMMENT; newComment (); addComment (); skip ())
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
       addComment (); skip ())
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
       minusComment (); 
                             if isZeroComment () then YYBEGIN INITIAL else (); 
                             skip ())
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;  skip ())
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         lexErr ((yypos, yypos), 
                                     ["bad character `", 
                                     String.toString yytext, "'"]);
                             continue ()
      end
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of INITIAL => ( T.EOF)
                    | BLOCKCOMMENT =>
                        ( lexErr ((yypos, yypos), 
                                     ["EOF in block comment"]); 
                             T.EOF)
                    | LINECOMMENT => ( T.EOF)
                    | INQUOTE =>
                        ( lexErr ((yypos, yypos), ["EOF in string"]); T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ89(strm', lastMatch)
              else yyQ88(strm', lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx7C
              then yyQ54(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction9(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction9(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction9(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ58(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ57(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ56(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction8(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ59(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction7(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction7(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ61(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ60(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction6(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ62(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction5(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction5(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction5(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ64(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ63(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction4(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction4(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ67(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction3(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction3(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ69(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ68(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp = 0wx30
                      then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                    else if inp < 0wx30
                      then yyAction39(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = 0wx5B
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ66(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6C
                  then yyQ65(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction2(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ72(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ71(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ70(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction1(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ75(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ74(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx62
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < 0wx62
              then if inp = 0wx60
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ73(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction39(strm, yyNO_MATCH)
                      else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ55(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction38(strm, yyNO_MATCH)
                      else yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx3A
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction38(strm, yyNO_MATCH)
                      else yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = 0wx60
              then yyAction38(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5F
                  then yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ76(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ78(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < 0wx3E
              then if inp = 0wx3D
                  then yyQ77(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ79(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3B
              then yyAction54(strm, yyNO_MATCH)
            else if inp < 0wx3B
              then if inp = 0wx3A
                  then yyQ80(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
                  else yyAction54(strm, yyNO_MATCH)
            else if inp = 0wx3D
              then yyQ81(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ82(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction40(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ82(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ82(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction40(strm, yyNO_MATCH)
            else if inp <= 0wx39
              then yyQ82(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2B
              then yyAction28(strm, yyNO_MATCH)
            else if inp < 0wx2B
              then if inp = 0wx2A
                  then yyQ83(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = 0wx2F
              then yyQ84(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ85(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx26
              then yyQ86(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ87(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of INITIAL => ( T.EOF)
                    | BLOCKCOMMENT =>
                        ( lexErr ((yypos, yypos), 
                                     ["EOF in block comment"]); 
                             T.EOF)
                    | LINECOMMENT => ( T.EOF)
                    | INQUOTE =>
                        ( lexErr ((yypos, yypos), ["EOF in string"]); T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ37(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx2A
                  then yyQ27(strm', lastMatch)
                else if inp < 0wx2A
                  then if inp = 0wx23
                      then yyQ19(strm', lastMatch)
                    else if inp < 0wx23
                      then if inp = 0wx20
                          then yyQ20(strm', lastMatch)
                        else if inp < 0wx20
                          then if inp = 0wx9
                              then yyQ20(strm', lastMatch)
                            else if inp < 0wx9
                              then yyQ19(strm', lastMatch)
                            else if inp <= 0wxD
                              then yyQ20(strm', lastMatch)
                              else yyQ19(strm', lastMatch)
                        else if inp = 0wx21
                          then yyQ21(strm', lastMatch)
                          else yyQ22(strm', lastMatch)
                    else if inp = 0wx27
                      then yyQ19(strm', lastMatch)
                    else if inp < 0wx27
                      then if inp = 0wx25
                          then yyQ23(strm', lastMatch)
                        else if inp = 0wx26
                          then yyQ24(strm', lastMatch)
                          else yyQ19(strm', lastMatch)
                    else if inp = 0wx28
                      then yyQ25(strm', lastMatch)
                      else yyQ26(strm', lastMatch)
                else if inp = 0wx30
                  then yyQ32(strm', lastMatch)
                else if inp < 0wx30
                  then if inp = 0wx2D
                      then yyQ30(strm', lastMatch)
                    else if inp < 0wx2D
                      then if inp = 0wx2B
                          then yyQ28(strm', lastMatch)
                          else yyQ29(strm', lastMatch)
                    else if inp = 0wx2E
                      then yyQ19(strm', lastMatch)
                      else yyQ31(strm', lastMatch)
                else if inp = 0wx3C
                  then yyQ35(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx3A
                      then yyQ33(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ34(strm', lastMatch)
                      else yyQ32(strm', lastMatch)
                else if inp = 0wx3D
                  then yyQ36(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = 0wx67
              then yyQ42(strm', lastMatch)
            else if inp < 0wx67
              then if inp = 0wx60
                  then yyQ19(strm', lastMatch)
                else if inp < 0wx60
                  then if inp = 0wx5D
                      then yyQ39(strm', lastMatch)
                    else if inp < 0wx5D
                      then if inp = 0wx5B
                          then yyQ38(strm', lastMatch)
                        else if inp = 0wx5C
                          then yyQ19(strm', lastMatch)
                          else yyQ37(strm', lastMatch)
                    else if inp = 0wx5E
                      then yyQ40(strm', lastMatch)
                      else yyQ41(strm', lastMatch)
                else if inp = 0wx64
                  then yyQ44(strm', lastMatch)
                else if inp < 0wx64
                  then if inp = 0wx63
                      then yyQ43(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = 0wx65
                  then yyQ45(strm', lastMatch)
                  else yyQ46(strm', lastMatch)
            else if inp = 0wx70
              then yyQ42(strm', lastMatch)
            else if inp < 0wx70
              then if inp = 0wx6C
                  then yyQ48(strm', lastMatch)
                else if inp < 0wx6C
                  then if inp = 0wx69
                      then yyQ47(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = 0wx6F
                  then yyQ49(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = 0wx7B
              then yyQ51(strm', lastMatch)
            else if inp < 0wx7B
              then if inp = 0wx74
                  then yyQ50(strm', lastMatch)
                  else yyQ42(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ53(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ52(strm', lastMatch)
              else yyQ19(strm', lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ13(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ13(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ16(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ16(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx31
              then yyQ13(strm', lastMatch)
            else if inp < 0wx31
              then if inp = 0wx30
                  then yyQ18(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ13(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx31
              then yyQ16(strm', lastMatch)
            else if inp < 0wx31
              then if inp = 0wx30
                  then yyQ17(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ16(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyAction46(strm, yyNO_MATCH)
            else if inp < 0wx5D
              then if inp = 0wx30
                  then yyQ14(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx22
                      then yyQ13(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                      else yyAction46(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction46(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ15(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                else if inp = 0wx5C
                  then yyQ13(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ13(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx6E
                  then yyQ13(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
                  else yyAction46(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ13(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of INITIAL => ( T.EOF)
                    | BLOCKCOMMENT =>
                        ( lexErr ((yypos, yypos), 
                                     ["EOF in block comment"]); 
                             T.EOF)
                    | LINECOMMENT => ( T.EOF)
                    | INQUOTE =>
                        ( lexErr ((yypos, yypos), ["EOF in string"]); T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx22
              then yyQ11(strm', lastMatch)
            else if inp < 0wx22
              then if inp = 0wx9
                  then yyQ10(strm', lastMatch)
                else if inp < 0wx9
                  then yyQ9(strm', lastMatch)
                else if inp <= 0wxD
                  then yyQ10(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ12(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2A
              then yyQ7(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ8(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(!(yystrm))
              then let
                val yycolno = ref(yygetcolNo(!(yystrm)))
                val yylineno = ref(yygetlineNo(!(yystrm)))
                in
                  (case (!(yyss))
                   of INITIAL => ( T.EOF)
                    | BLOCKCOMMENT =>
                        ( lexErr ((yypos, yypos), 
                                     ["EOF in block comment"]); 
                             T.EOF)
                    | LINECOMMENT => ( T.EOF)
                    | INQUOTE =>
                        ( lexErr ((yypos, yypos), ["EOF in string"]); T.EOF)
                  (* end case *))
                end
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2B
              then yyQ4(strm', lastMatch)
            else if inp < 0wx2B
              then if inp = 0wx2A
                  then yyQ5(strm', lastMatch)
                  else yyQ4(strm', lastMatch)
            else if inp = 0wx2F
              then yyQ6(strm', lastMatch)
              else yyQ4(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of BLOCKCOMMENT => yyQ0(!(yystrm), yyNO_MATCH)
    | INQUOTE => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
    | LINECOMMENT => yyQ3(!(yystrm), yyNO_MATCH)
  (* end case *))
end
end
            and skip() = (yystartPos := yygetPos(); 
			  yylastwasnref := ULexBuffer.lastWasNL (!yystrm);
			  continue())
	    in (continue(), (!yystartPos, yygetPos()-1), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = AntlrStreamPos.pos
    type span = AntlrStreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm 
(yyarg as  lexErr)(STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex 
yyarg(yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm 
yyarg(STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (yystreamify' 0 input, ref NONE), INITIAL)
    fun streamifyReader readFn strm = (STRM (yystreamifyReader' 0 readFn strm, ref NONE), 
				       INITIAL)
    fun streamifyInstream strm = (STRM (yystreamifyInstream' 0 strm, ref NONE), 
				  INITIAL)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end

