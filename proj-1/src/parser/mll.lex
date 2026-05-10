(* mll.lex
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * ML-Ulex specification for ML Lite.
 *)

%name MLLLex;

%let nonz  = [1-9];
%let num   = {nonz} | 0;
%let number = {num}+;
%let lower = [a-z];
%let upper = [A-Z];
%let idchar = {upper} | {lower} | {num} | "_";
%let id = {idchar}*;
%let lid  = {lower}{id};
%let uid = {upper}{id};
%let illegalwhite = \n | \r | \t | \v | \f;
%let whitespace = " " | {illegalwhite};
%let escape = \\(n | t | r | \\ | \" | {num}{3});


%arg (lexErr);

%defs(

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

);

(* INQUOTE for string scanning; 
 * LINECOMMENT for line comment scanning; 
 * BLOCKCOMMENT for block comment scanning
 *)
%states INQUOTE LINECOMMENT BLOCKCOMMENT;

<INITIAL> {whitespace}   => (skip ());
<INITIAL> "case"         => (T.KW_case);
<INITIAL> "data"         => (T.KW_data);
<INITIAL> "else"         => (T.KW_else);
<INITIAL> "end"          => (T.KW_end);
<INITIAL> "fun"          => (T.KW_fun);
<INITIAL> "if"           => (T.KW_if);
<INITIAL> "let"          => (T.KW_let);
<INITIAL> "of"           => (T.KW_of);
<INITIAL> "then"         => (T.KW_then);
<INITIAL> "("            => (T.LP);
<INITIAL> ")"            => (T.RP);
<INITIAL> "["            => (T.LB);
<INITIAL> "]"            => (T.RB);
<INITIAL> "{"            => (T.LCB);
<INITIAL> "}"            => (T.RCB);
<INITIAL> ":="           => (T.ASSIGN);
<INITIAL> "||"           => (T.ORELSE);
<INITIAL> "&&"           => (T.ANDALSO);
<INITIAL> "=="           => (T.EQEQ);
<INITIAL> "!="           => (T.NEQ);
<INITIAL> "<="           => (T.LTEQ);
<INITIAL> "<"            => (T.LT);
<INITIAL> "::"           => (T.CONS);
<INITIAL> "^"            => (T.CONCAT);
<INITIAL> "+"            => (T.PLUS);
<INITIAL> "-"            => (T.MINUS);
<INITIAL> "*"            => (T.TIMES);
<INITIAL> "/"            => (T.DIV);
<INITIAL> "%"            => (T.MOD);
<INITIAL> "!"            => (T.DEREF);
<INITIAL> "="            => (T.EQ);
<INITIAL> ","            => (T.COMMA);
<INITIAL> ";"            => (T.SEMI);
<INITIAL> "|"            => (T.BAR);
<INITIAL> "->"           => (T.ARROW);
<INITIAL> "=>"           => (T.DARROW);
<INITIAL> "_"            => (T.WILD);
<INITIAL> {uid}          => (T.UID (Atom.atom yytext));
<INITIAL> {lid}          => (T.LID (Atom.atom yytext));
<INITIAL> {number}       => (T.NUMBER (valOf (IntInf.fromString yytext))); 
<INITIAL> "\""           => (YYBEGIN INQUOTE; new (); continue ());
<INQUOTE> "\""           => (YYBEGIN INITIAL; T.STRING (get ()));
<INQUOTE> {illegalwhite} => (lexErr ((yypos, yypos), 
                                     ["illegal whitespace in string"]); 
                             continue ());
<INQUOTE> <<EOF>>        => (lexErr ((yypos, yypos), ["EOF in string"]); T.EOF);
<INQUOTE> "\\000"        => (lexErr ((yypos, yypos), 
                                     ["illegal escape code \\000"]); 
                             continue ());
<INQUOTE> {escape}       => (add (valOf (String.fromString yytext)); 
                             continue ());
<INQUOTE> .              => (add yytext; continue ());
<INITIAL> "//"           => (YYBEGIN LINECOMMENT; skip ());
<LINECOMMENT> "\n"       => (YYBEGIN INITIAL; skip ());
<LINECOMMENT> <<EOF>>    => (T.EOF);
<LINECOMMENT> .          => (skip ());
<INITIAL> "/*"           => (YYBEGIN BLOCKCOMMENT; newComment (); addComment (); skip ());
<BLOCKCOMMENT> "/*"      => (addComment (); skip ());
<BLOCKCOMMENT> "*/"      => (minusComment (); 
                             if isZeroComment () then YYBEGIN INITIAL else (); 
                             skip ());
<BLOCKCOMMENT> <<EOF>>   => (lexErr ((yypos, yypos), 
                                     ["EOF in block comment"]); 
                             T.EOF);
<BLOCKCOMMENT> .         => (skip ());
<INITIAL> <<EOF>>        => (T.EOF);
<INITIAL> .              => (lexErr ((yypos, yypos), 
                                     ["bad character `", 
                                     String.toString yytext, "'"]);
                             continue ());
    