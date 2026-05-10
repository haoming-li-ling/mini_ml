(* mll-tokens.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * The representation of tokens in the ML Lite scanner.
 *)

structure MLLTokens =
  struct
    datatype token
      = KW_case		        (* "case" *)
      | KW_data		        (* "data" *)
      | KW_else		        (* "else" *)
      | KW_end		        (* "end" *)
      | KW_fun		        (* "fun" *)
      | KW_if		        (* "if" *)
      | KW_let		        (* "let" *)
      | KW_of		        (* "of" *)
      | KW_then		        (* "then" *)
      | LP                      (* "(" *)
      | RP                      (* ")" *)
      | LB                      (* "[" *)
      | RB                      (* "]" *)
      | LCB                     (* "{" *)
      | RCB                     (* "}" *)
      | ASSIGN                  (* ":=" *)
      | ORELSE                  (* "||" *)
      | ANDALSO                 (* "&&" *)
      | EQEQ                    (* "==" *)
      | NEQ                     (* "!=" *)
      | LTEQ                    (* "<=" *)
      | LT                      (* "<" *)
      | CONS                    (* "::" *)
      | CONCAT                  (* "^" *)
      | PLUS                    (* "+" *)
      | MINUS                   (* "-" *)
      | TIMES                   (* "*" *)
      | DIV                     (* "/" *)
      | MOD                     (* "%" *)
      | DEREF                   (* "!" *)
      | EQ                      (* "=" *)
      | COMMA                   (* "," *)
      | SEMI                    (* ";" *)
      | BAR                     (* "|" *)
      | ARROW                   (* "->" *)
      | DARROW                  (* "=>" *)
      | WILD                    (* "_" *)
      | UID of Atom.atom        (* leading-upper-case identifiers *)
      | LID of Atom.atom        (* leading-lower-case identifiers *)
      | NUMBER of IntInf.int    (* integer literals *)
      | STRING of string        (* string literals *)
      | EOF
    fun toString tok = (case (tok)
           of (KW_case) => "case"
            | (KW_data) => "data"
            | (KW_else) => "else"
            | (KW_end) => "end"
            | (KW_fun) => "fun"
            | (KW_if) => "if"
            | (KW_let) => "let"
            | (KW_of) => "of"
            | (KW_then) => "then"
            | (LP) => "("
            | (RP) => ")"
            | (LB) => "["
            | (RB) => "]"
            | (LCB) => "{"
            | (RCB) => "}"
            | (ASSIGN) => ":="
            | (ORELSE) => "||"
            | (ANDALSO) => "&&"
            | (EQEQ) => "=="
            | (NEQ) => "!="
            | (LTEQ) => "<="
            | (LT) => "<"
            | (CONS) => "::"
            | (CONCAT) => "^"
            | (PLUS) => "+"
            | (MINUS) => "-"
            | (TIMES) => "*"
            | (DIV) => "/"
            | (MOD) => "%"
            | (DEREF) => "!"
            | (EQ) => "="
            | (COMMA) => ","
            | (SEMI) => ";"
            | (BAR) => "|"
            | (ARROW) => "->"
            | (DARROW) => "=>"
            | (WILD) => "_"
            | (UID(_)) => "UID"
            | (LID(_)) => "LID"
            | (NUMBER(_)) => "NUMBER"
            | (STRING(_)) => "STRING"
            | (EOF) => "EOF"
          (* end case *))

  end (* MLLTokens *)