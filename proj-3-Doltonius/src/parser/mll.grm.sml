structure MLLTokens =
  struct
    datatype token
      = KW_case
      | KW_data
      | KW_else
      | KW_end
      | KW_fun
      | KW_if
      | KW_let
      | KW_of
      | KW_then
      | LP
      | RP
      | LB
      | RB
      | LCB
      | RCB
      | ASSIGN
      | ORELSE
      | ANDALSO
      | EQEQ
      | NEQ
      | LTEQ
      | LT
      | CONS
      | CONCAT
      | PLUS
      | MINUS
      | TIMES
      | DIV
      | MOD
      | DEREF
      | EQ
      | COMMA
      | SEMI
      | BAR
      | ARROW
      | DARROW
      | WILD
      | UID of Atom.atom
      | LID of Atom.atom
      | NUMBER of IntInf.int
      | STRING of string
      | EOF
    val allToks = [
            KW_case, KW_data, KW_else, KW_end, KW_fun, KW_if, KW_let, KW_of, KW_then, LP, RP, LB, RB, LCB, RCB, ASSIGN, ORELSE, ANDALSO, EQEQ, NEQ, LTEQ, LT, CONS, CONCAT, PLUS, MINUS, TIMES, DIV, MOD, DEREF, EQ, COMMA, SEMI, BAR, ARROW, DARROW, WILD, EOF
           ]
    fun toString tok =
(case (tok)
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
    fun isKW tok =
(case (tok)
 of (KW_case) => false
  | (KW_data) => false
  | (KW_else) => false
  | (KW_end) => false
  | (KW_fun) => false
  | (KW_if) => false
  | (KW_let) => false
  | (KW_of) => false
  | (KW_then) => false
  | (LP) => false
  | (RP) => false
  | (LB) => false
  | (RB) => false
  | (LCB) => false
  | (RCB) => false
  | (ASSIGN) => false
  | (ORELSE) => false
  | (ANDALSO) => false
  | (EQEQ) => false
  | (NEQ) => false
  | (LTEQ) => false
  | (LT) => false
  | (CONS) => false
  | (CONCAT) => false
  | (PLUS) => false
  | (MINUS) => false
  | (TIMES) => false
  | (DIV) => false
  | (MOD) => false
  | (DEREF) => false
  | (EQ) => false
  | (COMMA) => false
  | (SEMI) => false
  | (BAR) => false
  | (ARROW) => false
  | (DARROW) => false
  | (WILD) => false
  | (UID(_)) => false
  | (LID(_)) => false
  | (NUMBER(_)) => false
  | (STRING(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* MLLTokens *)

functor MLLParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
MLLTokens
    structure UserCode =
      struct

  structure PT = ParseTree
  structure Op = OpNames

  type pos = Error.pos

  fun opt2list NONE = []
    | opt2list (SOME l) = l

  fun mark con (span, tr) = con{span = span, tree = tr}
  val markTy = mark PT.TyMark
  val markExp = mark PT.ExpMark
  val markPat = mark PT.PatMark

  fun mkCondExp mkExp (lPos : pos, lhs : PT.exp, rhs : (PT.exp * pos) list) = let
        fun mk (lhs, []) = lhs
          | mk (lhs, (e, rPos)::r) = mk (markExp ((lPos, rPos), mkExp(lhs, e)), r)
        in
          mk (lhs, rhs)
        end


  fun mkLBinExp (lPos : pos, lhs : PT.exp, rhs : (PT.id * PT.exp * pos) list) = let
        fun mk (lhs, []) = lhs
          | mk (lhs, (rator, e, rPos)::r) =
              mk (markExp ((lPos, rPos), PT.ExpBin(lhs, rator, e)), r)
        in
          mk (lhs, rhs)
        end


  fun mkRBinExp (lPos : pos, lhs : PT.exp, rhs : (PT.id * pos * PT.exp) list, rPos : pos) = let
        fun mk (_, lhs, []) = lhs
          | mk (lPos, lhs, (rator, lPos', e)::r) = let
              val rhs = mk (lPos', e, r)
              in
                markExp ((lPos, rPos), PT.ExpBin(lhs, rator, rhs))
              end
        in
          mk (lPos, lhs, rhs)
        end


  fun mkListConsExp (lPos : pos, lhs : PT.exp, rhs : (pos * PT.exp) list, rPos : pos) = let
        fun mk (_, lhs, []) = lhs
          | mk (lPos, lhs, (lPos', e)::r) = let
              val rhs = mk (lPos', e, r)
              in
                markExp ((lPos, rPos), PT.ExpListCons(lhs, rhs))
              end
        in
          mk (lPos, lhs, rhs)
        end

fun Program_PROD_1_ACT (Prog, Prog_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.ProgMark (FULL_SPAN, PT.Prog Prog))
fun Prog_PROD_1_ACT (SR, Exp, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of NONE => ([], Exp)
                      | SOME(dcls, exp) => let
                          val dcl = mark PT.DclMark (Exp_SPAN, PT.DclVal(PT.BindExp Exp))
                          in
                            (dcl :: dcls, exp)
                          end
                    )
fun Prog_PROD_2_ACT (TopDcl, Prog, SEMI, TopDcl_SPAN : (Lex.pos * Lex.pos), Prog_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (let val (dcls, exp) = Prog in (TopDcl::dcls, exp) end)
fun TopDcl_PROD_1_ACT (DataDcl, DataDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.DclMark (FULL_SPAN, DataDcl))
fun TopDcl_PROD_2_ACT (ValBind, ValBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.DclMark (FULL_SPAN, PT.DclVal ValBind))
fun DataDcl_PROD_1_ACT (EQ, SR, TypeParams, KW_data, ConDcl, UID, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TypeParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.DclData(UID, opt2list TypeParams, ConDcl::SR))
fun ConDcl_PROD_1_ACT (SR, UID, SR_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.ConMark (FULL_SPAN, PT.Con(UID, SR)))
fun TypeParams_PROD_1_ACT (LB, SR, RB, LID, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (LID :: SR)
fun Type_PROD_1_ACT (SR, TupleType, SR_SPAN : (Lex.pos * Lex.pos), TupleType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of NONE => TupleType
                      | SOME ty => markTy (FULL_SPAN, PT.TyFun(TupleType, ty))
                    )
fun TupleType_PROD_1_ACT (SR, AtomicType, SR_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                 of [] => AtomicType
                  | _ => markTy (FULL_SPAN, PT.TyTuple(AtomicType :: SR))
                )
fun AtomicType_PROD_1_ACT (LP, RP, Type, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Type)
fun AtomicType_PROD_2_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.TyVar LID))
fun AtomicType_PROD_3_ACT (TypeArgs, UID, TypeArgs_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markTy (FULL_SPAN, PT.TyCon(UID, opt2list TypeArgs)))
fun TypeArgs_PROD_1_ACT (LB, SR, RB, Type, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Type :: SR)
fun ValBind_PROD_1_ACT (EQ, KW_let, SimplePat, Exp, EQ_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.BindMark (FULL_SPAN, PT.BindVal(SimplePat, Exp)))
fun ValBind_PROD_2_ACT (EQ, KW_fun, LID, AtomicPat, Exp, EQ_SPAN : (Lex.pos * Lex.pos), KW_fun_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.BindMark (FULL_SPAN, PT.BindFun(LID, AtomicPat, Exp)))
fun Exp_PROD_1_ACT (Exp1, Exp2, Exp3, KW_if, KW_else, KW_then, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), Exp3_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExp (FULL_SPAN, PT.ExpIf(Exp1, Exp2, Exp3)))
fun Exp_PROD_2_ACT (AssignExp, AssignExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AssignExp)
fun AssignExp_PROD_1_ACT (SR, OrElseExp, SR_SPAN : (Lex.pos * Lex.pos), OrElseExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of SOME exp => markExp (FULL_SPAN, PT.ExpBin(OrElseExp, Op.asgnId, exp))
                      | NONE => OrElseExp
                    )
fun OrElseExp_PROD_1_SUBRULE_1_PROD_1_ACT (AndAlsoExp, ORELSE, AndAlsoExp_SPAN : (Lex.pos * Lex.pos), ORELSE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AndAlsoExp, #2 AndAlsoExp_SPAN)
fun OrElseExp_PROD_1_ACT (SR, AndAlsoExp, SR_SPAN : (Lex.pos * Lex.pos), AndAlsoExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkCondExp PT.ExpOrElse (#1 AndAlsoExp_SPAN, AndAlsoExp, SR))
fun AndAlsoExp_PROD_1_SUBRULE_1_PROD_1_ACT (RelExp, ANDALSO, RelExp_SPAN : (Lex.pos * Lex.pos), ANDALSO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (RelExp, #2 RelExp_SPAN)
fun AndAlsoExp_PROD_1_ACT (SR, RelExp, SR_SPAN : (Lex.pos * Lex.pos), RelExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkCondExp PT.ExpAndAlso (#1 RelExp_SPAN, RelExp, SR))
fun RelExp_PROD_1_SUBRULE_1_PROD_1_ACT (ListExp, RelOp, ListExp_SPAN : (Lex.pos * Lex.pos), RelOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (RelOp, ListExp, #2 ListExp_SPAN)
fun RelExp_PROD_1_ACT (SR, ListExp, SR_SPAN : (Lex.pos * Lex.pos), ListExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 ListExp_SPAN, ListExp, SR))
fun RelOp_PROD_1_ACT (EQEQ, EQEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.eqlId)
fun RelOp_PROD_2_ACT (NEQ, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.neqId)
fun RelOp_PROD_3_ACT (LT, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.ltId)
fun RelOp_PROD_4_ACT (LTEQ, LTEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.lteId)
fun ListExp_PROD_1_SUBRULE_1_PROD_1_ACT (CONS, AddExp, CONS_SPAN : (Lex.pos * Lex.pos), AddExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (#1 AddExp_SPAN, AddExp)
fun ListExp_PROD_1_ACT (SR, AddExp, SR_SPAN : (Lex.pos * Lex.pos), AddExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkListConsExp(#1 AddExp_SPAN, AddExp, SR, #2 FULL_SPAN))
fun AddExp_PROD_1_SUBRULE_1_PROD_1_ACT (MulExp, AddOp, MulExp_SPAN : (Lex.pos * Lex.pos), AddOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AddOp, MulExp, #2 MulExp_SPAN)
fun AddExp_PROD_1_ACT (SR, MulExp, SR_SPAN : (Lex.pos * Lex.pos), MulExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp(#1 FULL_SPAN, MulExp, SR))
fun AddOp_PROD_1_ACT (PLUS, PLUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.addId)
fun AddOp_PROD_2_ACT (MINUS, MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.subId)
fun AddOp_PROD_3_ACT (CONCAT, CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Op.strcatId)
fun MulExp_PROD_1_SUBRULE_1_PROD_1_ACT (PrefixExp, MulOp, PrefixExp_SPAN : (Lex.pos * Lex.pos), MulOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (MulOp, PrefixExp, #2 PrefixExp_SPAN)
fun MulExp_PROD_1_ACT (SR, PrefixExp, SR_SPAN : (Lex.pos * Lex.pos), PrefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mkLBinExp (#1 PrefixExp_SPAN, PrefixExp, SR))
fun MulOp_PROD_1_ACT (TIMES, TIMES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.mulId)
fun MulOp_PROD_2_ACT (DIV, DIV_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.divId)
fun MulOp_PROD_3_ACT (MOD, MOD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.modId)
fun PrefixExp_PROD_1_ACT (ApplyExp, UnaryOp, ApplyExp_SPAN : (Lex.pos * Lex.pos), UnaryOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (let
                    val rhsPos = #2 ApplyExp_SPAN
                    fun mk ((id, lhsPos), exp) =
                          markExp ((lhsPos, rhsPos), PT.ExpUn(id, exp))
                    in
                      List.foldr mk ApplyExp UnaryOp
                    end)
fun UnaryOp_PROD_1_ACT (MINUS, MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.negId, #1 FULL_SPAN)
fun UnaryOp_PROD_2_ACT (DEREF, DEREF_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (OpNames.derefId, #1 FULL_SPAN)
fun ApplyExp_PROD_1_SUBRULE_1_PROD_1_ACT (AtomicExp, AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (#2 AtomicExp_SPAN, AtomicExp)
fun ApplyExp_PROD_1_ACT (SR, AtomicExp, SR_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (let
                    fun mkApp (e, []) = e
                      | mkApp (e, (rPos, e')::r) =
                          mkApp (markExp ((#1 AtomicExp_SPAN, rPos), PT.ExpApp(e, e')), r)
                    in
                      mkApp (AtomicExp, SR)
                    end)
fun AtomicExp_PROD_1_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExp (FULL_SPAN, PT.ExpVar LID))
fun AtomicExp_PROD_2_ACT (UID, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExp (FULL_SPAN, PT.ExpCon UID))
fun AtomicExp_PROD_3_ACT (NUMBER, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExp (FULL_SPAN, PT.ExpInt NUMBER))
fun AtomicExp_PROD_4_ACT (STRING, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExp (FULL_SPAN, PT.ExpStr STRING))
fun AtomicExp_PROD_5_SUBRULE_1_PROD_1_ACT (LP, SR, Exp, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Exp :: SR)
fun AtomicExp_PROD_5_ACT (LP, SR, RP, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case opt2list SR
                     of [exp] => exp
                      | exps => markExp (FULL_SPAN, PT.ExpTuple exps)
                    )
fun AtomicExp_PROD_6_ACT (RCB, LCB, Scope, RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExp (FULL_SPAN, PT.ExpScope Scope))
fun AtomicExp_PROD_7_ACT (KW_end, KW_of, Exp, KW_case, MatchCase, KW_end_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), KW_case_SPAN : (Lex.pos * Lex.pos), MatchCase_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markExp (FULL_SPAN, PT.ExpCase(Exp, MatchCase)))
fun Scope_PROD_1_ACT (ValBind, SEMI, Scope, ValBind_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (let val (defs, exp) = Scope
                    in
                      (ValBind :: defs, exp)
                    end)
fun Scope_PROD_2_ACT (SR, Exp, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of NONE => ([], Exp)
                      | SOME(defs, exp) => (PT.BindExp Exp :: defs, exp)
                    )
fun MatchCase_PROD_1_ACT (Pat, RCB, LCB, DARROW, Scope, Pat_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.RuleMark (FULL_SPAN, PT.RuleCase(Pat, Scope)))
fun Pat_PROD_1_ACT (AtomicPat, AtomicPat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (AtomicPat)
fun Pat_PROD_2_ACT (SimplePat, UID, SimplePat_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markPat (FULL_SPAN, PT.PatCon(UID, SimplePat)))
fun Pat_PROD_3_ACT (CONS, SimplePat1, SimplePat2, CONS_SPAN : (Lex.pos * Lex.pos), SimplePat1_SPAN : (Lex.pos * Lex.pos), SimplePat2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markPat (
                      FULL_SPAN,
                      PT.PatListCons(SimplePat1, SimplePat2)))
fun AtomicPat_PROD_1_ACT (SimplePat, SimplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SimplePat)
fun AtomicPat_PROD_2_ACT (LP, SR, RP, SimplePat, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (case SR
                     of [] => SimplePat
                      | pats => markPat (FULL_SPAN, PT.PatTuple(SimplePat :: pats))
                    )
fun SimplePat_PROD_1_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markPat (FULL_SPAN, PT.PatVar LID))
fun SimplePat_PROD_2_ACT (WILD, WILD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markPat (FULL_SPAN, PT.PatWild))
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchKW_case strm = (case (lex(strm))
 of (Tok.KW_case, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_data strm = (case (lex(strm))
 of (Tok.KW_data, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_else strm = (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_end strm = (case (lex(strm))
 of (Tok.KW_end, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_fun strm = (case (lex(strm))
 of (Tok.KW_fun, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_if strm = (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_of strm = (case (lex(strm))
 of (Tok.KW_of, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_then strm = (case (lex(strm))
 of (Tok.KW_then, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLB strm = (case (lex(strm))
 of (Tok.LB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRB strm = (case (lex(strm))
 of (Tok.RB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLCB strm = (case (lex(strm))
 of (Tok.LCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRCB strm = (case (lex(strm))
 of (Tok.RCB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASSIGN strm = (case (lex(strm))
 of (Tok.ASSIGN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchORELSE strm = (case (lex(strm))
 of (Tok.ORELSE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchANDALSO strm = (case (lex(strm))
 of (Tok.ANDALSO, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQEQ strm = (case (lex(strm))
 of (Tok.EQEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNEQ strm = (case (lex(strm))
 of (Tok.NEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLTEQ strm = (case (lex(strm))
 of (Tok.LTEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLT strm = (case (lex(strm))
 of (Tok.LT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONS strm = (case (lex(strm))
 of (Tok.CONS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCONCAT strm = (case (lex(strm))
 of (Tok.CONCAT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDIV strm = (case (lex(strm))
 of (Tok.DIV, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchMOD strm = (case (lex(strm))
 of (Tok.MOD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDEREF strm = (case (lex(strm))
 of (Tok.DEREF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchBAR strm = (case (lex(strm))
 of (Tok.BAR, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchARROW strm = (case (lex(strm))
 of (Tok.ARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDARROW strm = (case (lex(strm))
 of (Tok.DARROW, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchWILD strm = (case (lex(strm))
 of (Tok.WILD, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchUID strm = (case (lex(strm))
 of (Tok.UID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLID strm = (case (lex(strm))
 of (Tok.LID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchNUMBER strm = (case (lex(strm))
 of (Tok.NUMBER(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSTRING strm = (case (lex(strm))
 of (Tok.STRING(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Program_NT) = 
let
fun SimplePat_NT (strm) = let
      fun SimplePat_PROD_1 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.SimplePat_PROD_1_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun SimplePat_PROD_2 (strm) = let
            val (WILD_RES, WILD_SPAN, strm') = matchWILD(strm)
            val FULL_SPAN = (#1(WILD_SPAN), #2(WILD_SPAN))
            in
              (UserCode.SimplePat_PROD_2_ACT (WILD_RES, WILD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.WILD, _, strm') => SimplePat_PROD_2(strm)
          | (Tok.LID(_), _, strm') => SimplePat_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun AtomicPat_NT (strm) = let
      fun AtomicPat_PROD_1 (strm) = let
            val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm)
            val FULL_SPAN = (#1(SimplePat_SPAN), #2(SimplePat_SPAN))
            in
              (UserCode.AtomicPat_PROD_1_ACT (SimplePat_RES, SimplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicPat_PROD_2 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm')
            fun AtomicPat_PROD_2_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(SimplePat_SPAN))
                  in
                    ((SimplePat_RES), FULL_SPAN, strm')
                  end
            fun AtomicPat_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicPat_PROD_2_SUBRULE_1_PRED, AtomicPat_PROD_2_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicPat_PROD_2_ACT (LP_RES, SR_RES, RP_RES, SimplePat_RES, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LP, _, strm') => AtomicPat_PROD_2(strm)
          | (Tok.WILD, _, strm') => AtomicPat_PROD_1(strm)
          | (Tok.LID(_), _, strm') => AtomicPat_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Pat_NT (strm) = let
      fun Pat_PROD_1 (strm) = let
            val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm)
            val FULL_SPAN = (#1(AtomicPat_SPAN), #2(AtomicPat_SPAN))
            in
              (UserCode.Pat_PROD_1_ACT (AtomicPat_RES, AtomicPat_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Pat_PROD_2 (strm) = let
            val (UID_RES, UID_SPAN, strm') = matchUID(strm)
            fun Pat_PROD_2_SUBRULE_1_NT (strm) = let
                  val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm)
                  val FULL_SPAN = (#1(SimplePat_SPAN), #2(SimplePat_SPAN))
                  in
                    ((SimplePat_RES), FULL_SPAN, strm')
                  end
            fun Pat_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.WILD, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SimplePat_RES, SimplePat_SPAN, strm') = EBNF.optional(Pat_PROD_2_SUBRULE_1_PRED, Pat_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(UID_SPAN), #2(SimplePat_SPAN))
            in
              (UserCode.Pat_PROD_2_ACT (SimplePat_RES, UID_RES, SimplePat_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Pat_PROD_3 (strm) = let
            val (SimplePat1_RES, SimplePat1_SPAN, strm') = SimplePat_NT(strm)
            val (CONS_RES, CONS_SPAN, strm') = matchCONS(strm')
            val (SimplePat2_RES, SimplePat2_SPAN, strm') = SimplePat_NT(strm')
            val FULL_SPAN = (#1(SimplePat1_SPAN), #2(SimplePat2_SPAN))
            in
              (UserCode.Pat_PROD_3_ACT (CONS_RES, SimplePat1_RES, SimplePat2_RES, CONS_SPAN : (Lex.pos * Lex.pos), SimplePat1_SPAN : (Lex.pos * Lex.pos), SimplePat2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.WILD, _, strm') =>
              (case (lex(strm'))
               of (Tok.DARROW, _, strm') => Pat_PROD_1(strm)
                | (Tok.CONS, _, strm') => Pat_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.DARROW, _, strm') => Pat_PROD_1(strm)
                | (Tok.CONS, _, strm') => Pat_PROD_3(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LP, _, strm') => Pat_PROD_1(strm)
          | (Tok.UID(_), _, strm') => Pat_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun UnaryOp_NT (strm) = let
      fun UnaryOp_PROD_1 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val FULL_SPAN = (#1(MINUS_SPAN), #2(MINUS_SPAN))
            in
              (UserCode.UnaryOp_PROD_1_ACT (MINUS_RES, MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun UnaryOp_PROD_2 (strm) = let
            val (DEREF_RES, DEREF_SPAN, strm') = matchDEREF(strm)
            val FULL_SPAN = (#1(DEREF_SPAN), #2(DEREF_SPAN))
            in
              (UserCode.UnaryOp_PROD_2_ACT (DEREF_RES, DEREF_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DEREF, _, strm') => UnaryOp_PROD_2(strm)
          | (Tok.MINUS, _, strm') => UnaryOp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun MulOp_NT (strm) = let
      fun MulOp_PROD_1 (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val FULL_SPAN = (#1(TIMES_SPAN), #2(TIMES_SPAN))
            in
              (UserCode.MulOp_PROD_1_ACT (TIMES_RES, TIMES_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_2 (strm) = let
            val (DIV_RES, DIV_SPAN, strm') = matchDIV(strm)
            val FULL_SPAN = (#1(DIV_SPAN), #2(DIV_SPAN))
            in
              (UserCode.MulOp_PROD_2_ACT (DIV_RES, DIV_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulOp_PROD_3 (strm) = let
            val (MOD_RES, MOD_SPAN, strm') = matchMOD(strm)
            val FULL_SPAN = (#1(MOD_SPAN), #2(MOD_SPAN))
            in
              (UserCode.MulOp_PROD_3_ACT (MOD_RES, MOD_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.MOD, _, strm') => MulOp_PROD_3(strm)
          | (Tok.TIMES, _, strm') => MulOp_PROD_1(strm)
          | (Tok.DIV, _, strm') => MulOp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun AddOp_NT (strm) = let
      fun AddOp_PROD_1 (strm) = let
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
            val FULL_SPAN = (#1(PLUS_SPAN), #2(PLUS_SPAN))
            in
              (UserCode.AddOp_PROD_1_ACT (PLUS_RES, PLUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AddOp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val FULL_SPAN = (#1(MINUS_SPAN), #2(MINUS_SPAN))
            in
              (UserCode.AddOp_PROD_2_ACT (MINUS_RES, MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AddOp_PROD_3 (strm) = let
            val (CONCAT_RES, CONCAT_SPAN, strm') = matchCONCAT(strm)
            val FULL_SPAN = (#1(CONCAT_SPAN), #2(CONCAT_SPAN))
            in
              (UserCode.AddOp_PROD_3_ACT (CONCAT_RES, CONCAT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.CONCAT, _, strm') => AddOp_PROD_3(strm)
          | (Tok.PLUS, _, strm') => AddOp_PROD_1(strm)
          | (Tok.MINUS, _, strm') => AddOp_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun RelOp_NT (strm) = let
      fun RelOp_PROD_1 (strm) = let
            val (EQEQ_RES, EQEQ_SPAN, strm') = matchEQEQ(strm)
            val FULL_SPAN = (#1(EQEQ_SPAN), #2(EQEQ_SPAN))
            in
              (UserCode.RelOp_PROD_1_ACT (EQEQ_RES, EQEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_2 (strm) = let
            val (NEQ_RES, NEQ_SPAN, strm') = matchNEQ(strm)
            val FULL_SPAN = (#1(NEQ_SPAN), #2(NEQ_SPAN))
            in
              (UserCode.RelOp_PROD_2_ACT (NEQ_RES, NEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_3 (strm) = let
            val (LT_RES, LT_SPAN, strm') = matchLT(strm)
            val FULL_SPAN = (#1(LT_SPAN), #2(LT_SPAN))
            in
              (UserCode.RelOp_PROD_3_ACT (LT_RES, LT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelOp_PROD_4 (strm) = let
            val (LTEQ_RES, LTEQ_SPAN, strm') = matchLTEQ(strm)
            val FULL_SPAN = (#1(LTEQ_SPAN), #2(LTEQ_SPAN))
            in
              (UserCode.RelOp_PROD_4_ACT (LTEQ_RES, LTEQ_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LTEQ, _, strm') => RelOp_PROD_4(strm)
          | (Tok.NEQ, _, strm') => RelOp_PROD_2(strm)
          | (Tok.EQEQ, _, strm') => RelOp_PROD_1(strm)
          | (Tok.LT, _, strm') => RelOp_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
fun ValBind_NT (strm) = let
      fun ValBind_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (SimplePat_RES, SimplePat_SPAN, strm') = SimplePat_NT(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(Exp_SPAN))
            in
              (UserCode.ValBind_PROD_1_ACT (EQ_RES, KW_let_RES, SimplePat_RES, Exp_RES, EQ_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), SimplePat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ValBind_PROD_2 (strm) = let
            val (KW_fun_RES, KW_fun_SPAN, strm') = matchKW_fun(strm)
            val (LID_RES, LID_SPAN, strm') = matchLID(strm')
            fun ValBind_PROD_2_SUBRULE_1_NT (strm) = let
                  val (AtomicPat_RES, AtomicPat_SPAN, strm') = AtomicPat_NT(strm)
                  val FULL_SPAN = (#1(AtomicPat_SPAN), #2(AtomicPat_SPAN))
                  in
                    ((AtomicPat_RES), FULL_SPAN, strm')
                  end
            fun ValBind_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LP, _, strm') => true
                    | (Tok.WILD, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (AtomicPat_RES, AtomicPat_SPAN, strm') = EBNF.posclos(ValBind_PROD_2_SUBRULE_1_PRED, ValBind_PROD_2_SUBRULE_1_NT, strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_fun_SPAN), #2(Exp_SPAN))
            in
              (UserCode.ValBind_PROD_2_ACT (EQ_RES, KW_fun_RES, LID_RES, AtomicPat_RES, Exp_RES, EQ_SPAN : (Lex.pos * Lex.pos), KW_fun_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), AtomicPat_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => ValBind_PROD_2(strm)
          | (Tok.KW_let, _, strm') => ValBind_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and Exp_NT (strm) = let
      fun Exp_PROD_1 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (Exp1_RES, Exp1_SPAN, strm') = Exp_NT(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (Exp2_RES, Exp2_SPAN, strm') = Exp_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (Exp3_RES, Exp3_SPAN, strm') = Exp_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(Exp3_SPAN))
            in
              (UserCode.Exp_PROD_1_ACT (Exp1_RES, Exp2_RES, Exp3_RES, KW_if_RES, KW_else_RES, KW_then_RES, Exp1_SPAN : (Lex.pos * Lex.pos), Exp2_SPAN : (Lex.pos * Lex.pos), Exp3_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Exp_PROD_2 (strm) = let
            val (AssignExp_RES, AssignExp_SPAN, strm') = AssignExp_NT(strm)
            val FULL_SPAN = (#1(AssignExp_SPAN), #2(AssignExp_SPAN))
            in
              (UserCode.Exp_PROD_2_ACT (AssignExp_RES, AssignExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_case, _, strm') => Exp_PROD_2(strm)
          | (Tok.LP, _, strm') => Exp_PROD_2(strm)
          | (Tok.LCB, _, strm') => Exp_PROD_2(strm)
          | (Tok.MINUS, _, strm') => Exp_PROD_2(strm)
          | (Tok.DEREF, _, strm') => Exp_PROD_2(strm)
          | (Tok.UID(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.LID(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.NUMBER(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => Exp_PROD_2(strm)
          | (Tok.KW_if, _, strm') => Exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and AssignExp_NT (strm) = let
      val (OrElseExp_RES, OrElseExp_SPAN, strm') = OrElseExp_NT(strm)
      fun AssignExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (ASSIGN_RES, ASSIGN_SPAN, strm') = matchASSIGN(strm)
            val (OrElseExp_RES, OrElseExp_SPAN, strm') = OrElseExp_NT(strm')
            val FULL_SPAN = (#1(ASSIGN_SPAN), #2(OrElseExp_SPAN))
            in
              ((OrElseExp_RES), FULL_SPAN, strm')
            end
      fun AssignExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ASSIGN, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(AssignExp_PROD_1_SUBRULE_1_PRED, AssignExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(OrElseExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.AssignExp_PROD_1_ACT (SR_RES, OrElseExp_RES, SR_SPAN : (Lex.pos * Lex.pos), OrElseExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and OrElseExp_NT (strm) = let
      val (AndAlsoExp_RES, AndAlsoExp_SPAN, strm') = AndAlsoExp_NT(strm)
      fun OrElseExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (ORELSE_RES, ORELSE_SPAN, strm') = matchORELSE(strm)
            val (AndAlsoExp_RES, AndAlsoExp_SPAN, strm') = AndAlsoExp_NT(strm')
            val FULL_SPAN = (#1(ORELSE_SPAN), #2(AndAlsoExp_SPAN))
            in
              (UserCode.OrElseExp_PROD_1_SUBRULE_1_PROD_1_ACT (AndAlsoExp_RES, ORELSE_RES, AndAlsoExp_SPAN : (Lex.pos * Lex.pos), ORELSE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun OrElseExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ORELSE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(OrElseExp_PROD_1_SUBRULE_1_PRED, OrElseExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AndAlsoExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.OrElseExp_PROD_1_ACT (SR_RES, AndAlsoExp_RES, SR_SPAN : (Lex.pos * Lex.pos), AndAlsoExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AndAlsoExp_NT (strm) = let
      val (RelExp_RES, RelExp_SPAN, strm') = RelExp_NT(strm)
      fun AndAlsoExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (ANDALSO_RES, ANDALSO_SPAN, strm') = matchANDALSO(strm)
            val (RelExp_RES, RelExp_SPAN, strm') = RelExp_NT(strm')
            val FULL_SPAN = (#1(ANDALSO_SPAN), #2(RelExp_SPAN))
            in
              (UserCode.AndAlsoExp_PROD_1_SUBRULE_1_PROD_1_ACT (RelExp_RES, ANDALSO_RES, RelExp_SPAN : (Lex.pos * Lex.pos), ANDALSO_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AndAlsoExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ANDALSO, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AndAlsoExp_PROD_1_SUBRULE_1_PRED, AndAlsoExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(RelExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.AndAlsoExp_PROD_1_ACT (SR_RES, RelExp_RES, SR_SPAN : (Lex.pos * Lex.pos), RelExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and RelExp_NT (strm) = let
      val (ListExp_RES, ListExp_SPAN, strm') = ListExp_NT(strm)
      fun RelExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (RelOp_RES, RelOp_SPAN, strm') = RelOp_NT(strm)
            val (ListExp_RES, ListExp_SPAN, strm') = ListExp_NT(strm')
            val FULL_SPAN = (#1(RelOp_SPAN), #2(ListExp_SPAN))
            in
              (UserCode.RelExp_PROD_1_SUBRULE_1_PROD_1_ACT (ListExp_RES, RelOp_RES, ListExp_SPAN : (Lex.pos * Lex.pos), RelOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun RelExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.EQEQ, _, strm') => true
              | (Tok.NEQ, _, strm') => true
              | (Tok.LTEQ, _, strm') => true
              | (Tok.LT, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(RelExp_PROD_1_SUBRULE_1_PRED, RelExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(ListExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.RelExp_PROD_1_ACT (SR_RES, ListExp_RES, SR_SPAN : (Lex.pos * Lex.pos), ListExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and ListExp_NT (strm) = let
      val (AddExp_RES, AddExp_SPAN, strm') = AddExp_NT(strm)
      fun ListExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (CONS_RES, CONS_SPAN, strm') = matchCONS(strm)
            val (AddExp_RES, AddExp_SPAN, strm') = AddExp_NT(strm')
            val FULL_SPAN = (#1(CONS_SPAN), #2(AddExp_SPAN))
            in
              (UserCode.ListExp_PROD_1_SUBRULE_1_PROD_1_ACT (CONS_RES, AddExp_RES, CONS_SPAN : (Lex.pos * Lex.pos), AddExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ListExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.CONS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(ListExp_PROD_1_SUBRULE_1_PRED, ListExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AddExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.ListExp_PROD_1_ACT (SR_RES, AddExp_RES, SR_SPAN : (Lex.pos * Lex.pos), AddExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AddExp_NT (strm) = let
      val (MulExp_RES, MulExp_SPAN, strm') = MulExp_NT(strm)
      fun AddExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (AddOp_RES, AddOp_SPAN, strm') = AddOp_NT(strm)
            val (MulExp_RES, MulExp_SPAN, strm') = MulExp_NT(strm')
            val FULL_SPAN = (#1(AddOp_SPAN), #2(MulExp_SPAN))
            in
              (UserCode.AddExp_PROD_1_SUBRULE_1_PROD_1_ACT (MulExp_RES, AddOp_RES, MulExp_SPAN : (Lex.pos * Lex.pos), AddOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AddExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.CONCAT, _, strm') => true
              | (Tok.PLUS, _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(AddExp_PROD_1_SUBRULE_1_PRED, AddExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(MulExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.AddExp_PROD_1_ACT (SR_RES, MulExp_RES, SR_SPAN : (Lex.pos * Lex.pos), MulExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and MulExp_NT (strm) = let
      val (PrefixExp_RES, PrefixExp_SPAN, strm') = PrefixExp_NT(strm)
      fun MulExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (MulOp_RES, MulOp_SPAN, strm') = MulOp_NT(strm)
            val (PrefixExp_RES, PrefixExp_SPAN, strm') = PrefixExp_NT(strm')
            val FULL_SPAN = (#1(MulOp_SPAN), #2(PrefixExp_SPAN))
            in
              (UserCode.MulExp_PROD_1_SUBRULE_1_PROD_1_ACT (PrefixExp_RES, MulOp_RES, PrefixExp_SPAN : (Lex.pos * Lex.pos), MulOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun MulExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | (Tok.DIV, _, strm') => true
              | (Tok.MOD, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(MulExp_PROD_1_SUBRULE_1_PRED, MulExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(PrefixExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.MulExp_PROD_1_ACT (SR_RES, PrefixExp_RES, SR_SPAN : (Lex.pos * Lex.pos), PrefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and PrefixExp_NT (strm) = let
      fun PrefixExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (UnaryOp_RES, UnaryOp_SPAN, strm') = UnaryOp_NT(strm)
            val FULL_SPAN = (#1(UnaryOp_SPAN), #2(UnaryOp_SPAN))
            in
              ((UnaryOp_RES), FULL_SPAN, strm')
            end
      fun PrefixExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.MINUS, _, strm') => true
              | (Tok.DEREF, _, strm') => true
              | _ => false
            (* end case *))
      val (UnaryOp_RES, UnaryOp_SPAN, strm') = EBNF.closure(PrefixExp_PROD_1_SUBRULE_1_PRED, PrefixExp_PROD_1_SUBRULE_1_NT, strm)
      val (ApplyExp_RES, ApplyExp_SPAN, strm') = ApplyExp_NT(strm')
      val FULL_SPAN = (#1(UnaryOp_SPAN), #2(ApplyExp_SPAN))
      in
        (UserCode.PrefixExp_PROD_1_ACT (ApplyExp_RES, UnaryOp_RES, ApplyExp_SPAN : (Lex.pos * Lex.pos), UnaryOp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and ApplyExp_NT (strm) = let
      val (AtomicExp_RES, AtomicExp_SPAN, strm') = AtomicExp_NT(strm)
      fun ApplyExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (AtomicExp_RES, AtomicExp_SPAN, strm') = AtomicExp_NT(strm)
            val FULL_SPAN = (#1(AtomicExp_SPAN), #2(AtomicExp_SPAN))
            in
              (UserCode.ApplyExp_PROD_1_SUBRULE_1_PROD_1_ACT (AtomicExp_RES, AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ApplyExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_case, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.LCB, _, strm') => true
              | (Tok.UID(_), _, strm') => true
              | (Tok.LID(_), _, strm') => true
              | (Tok.NUMBER(_), _, strm') => true
              | (Tok.STRING(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(ApplyExp_PROD_1_SUBRULE_1_PRED, ApplyExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AtomicExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.ApplyExp_PROD_1_ACT (SR_RES, AtomicExp_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomicExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AtomicExp_NT (strm) = let
      fun AtomicExp_PROD_1 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.AtomicExp_PROD_1_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_2 (strm) = let
            val (UID_RES, UID_SPAN, strm') = matchUID(strm)
            val FULL_SPAN = (#1(UID_SPAN), #2(UID_SPAN))
            in
              (UserCode.AtomicExp_PROD_2_ACT (UID_RES, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_3 (strm) = let
            val (NUMBER_RES, NUMBER_SPAN, strm') = matchNUMBER(strm)
            val FULL_SPAN = (#1(NUMBER_SPAN), #2(NUMBER_SPAN))
            in
              (UserCode.AtomicExp_PROD_3_ACT (NUMBER_RES, NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_4 (strm) = let
            val (STRING_RES, STRING_SPAN, strm') = matchSTRING(strm)
            val FULL_SPAN = (#1(STRING_SPAN), #2(STRING_SPAN))
            in
              (UserCode.AtomicExp_PROD_4_ACT (STRING_RES, STRING_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_5 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            fun AtomicExp_PROD_5_SUBRULE_1_NT (strm) = let
                  val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
                  fun AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                        
                        val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                        val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
                        val FULL_SPAN = (#1(COMMA_SPAN), #2(Exp_SPAN))
                        in
                          ((Exp_RES), FULL_SPAN, strm')
                        end
                  fun AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = 
                        (case (lex(strm))
                         of (Tok.COMMA, _, strm') => true
                          | _ => false
                        (* end case *))
                  val (SR_RES, SR_SPAN, strm') = EBNF.closure(AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_PRED, AtomicExp_PROD_5_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
                  val FULL_SPAN = (#1(Exp_SPAN), #2(SR_SPAN))
                  in
                    (UserCode.AtomicExp_PROD_5_SUBRULE_1_PROD_1_ACT (LP_RES, SR_RES, Exp_RES, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun AtomicExp_PROD_5_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_case, _, strm') => true
                    | (Tok.KW_if, _, strm') => true
                    | (Tok.LP, _, strm') => true
                    | (Tok.LCB, _, strm') => true
                    | (Tok.MINUS, _, strm') => true
                    | (Tok.DEREF, _, strm') => true
                    | (Tok.UID(_), _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | (Tok.NUMBER(_), _, strm') => true
                    | (Tok.STRING(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(AtomicExp_PROD_5_SUBRULE_1_PRED, AtomicExp_PROD_5_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicExp_PROD_5_ACT (LP_RES, SR_RES, RP_RES, LP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_6 (strm) = let
            val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
            val (Scope_RES, Scope_SPAN, strm') = Scope_NT(strm')
            val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
            val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
            in
              (UserCode.AtomicExp_PROD_6_ACT (RCB_RES, LCB_RES, Scope_RES, RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicExp_PROD_7 (strm) = let
            val (KW_case_RES, KW_case_SPAN, strm') = matchKW_case(strm)
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm')
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm')
            fun AtomicExp_PROD_7_SUBRULE_1_NT (strm) = let
                  val (MatchCase_RES, MatchCase_SPAN, strm') = MatchCase_NT(strm)
                  val FULL_SPAN = (#1(MatchCase_SPAN), #2(MatchCase_SPAN))
                  in
                    ((MatchCase_RES), FULL_SPAN, strm')
                  end
            fun AtomicExp_PROD_7_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LCB, _, strm') => true
                    | _ => false
                  (* end case *))
            val (MatchCase_RES, MatchCase_SPAN, strm') = EBNF.posclos(AtomicExp_PROD_7_SUBRULE_1_PRED, AtomicExp_PROD_7_SUBRULE_1_NT, strm')
            val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
            val FULL_SPAN = (#1(KW_case_SPAN), #2(KW_end_SPAN))
            in
              (UserCode.AtomicExp_PROD_7_ACT (KW_end_RES, KW_of_RES, Exp_RES, KW_case_RES, MatchCase_RES, KW_end_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), KW_case_SPAN : (Lex.pos * Lex.pos), MatchCase_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_case, _, strm') => AtomicExp_PROD_7(strm)
          | (Tok.LP, _, strm') => AtomicExp_PROD_5(strm)
          | (Tok.NUMBER(_), _, strm') => AtomicExp_PROD_3(strm)
          | (Tok.LID(_), _, strm') => AtomicExp_PROD_1(strm)
          | (Tok.UID(_), _, strm') => AtomicExp_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => AtomicExp_PROD_4(strm)
          | (Tok.LCB, _, strm') => AtomicExp_PROD_6(strm)
          | _ => fail()
        (* end case *))
      end
and MatchCase_NT (strm) = let
      val (LCB_RES, LCB_SPAN, strm') = matchLCB(strm)
      val (Pat_RES, Pat_SPAN, strm') = Pat_NT(strm')
      val (DARROW_RES, DARROW_SPAN, strm') = matchDARROW(strm')
      val (Scope_RES, Scope_SPAN, strm') = Scope_NT(strm')
      val (RCB_RES, RCB_SPAN, strm') = matchRCB(strm')
      val FULL_SPAN = (#1(LCB_SPAN), #2(RCB_SPAN))
      in
        (UserCode.MatchCase_PROD_1_ACT (Pat_RES, RCB_RES, LCB_RES, DARROW_RES, Scope_RES, Pat_SPAN : (Lex.pos * Lex.pos), RCB_SPAN : (Lex.pos * Lex.pos), LCB_SPAN : (Lex.pos * Lex.pos), DARROW_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and Scope_NT (strm) = let
      fun Scope_PROD_1 (strm) = let
            val (ValBind_RES, ValBind_SPAN, strm') = ValBind_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (Scope_RES, Scope_SPAN, strm') = Scope_NT(strm')
            val FULL_SPAN = (#1(ValBind_SPAN), #2(Scope_SPAN))
            in
              (UserCode.Scope_PROD_1_ACT (ValBind_RES, SEMI_RES, Scope_RES, ValBind_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), Scope_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Scope_PROD_2 (strm) = let
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
            fun Scope_PROD_2_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (Scope_RES, Scope_SPAN, strm') = Scope_NT(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(Scope_SPAN))
                  in
                    ((Scope_RES), FULL_SPAN, strm')
                  end
            fun Scope_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(Scope_PROD_2_SUBRULE_1_PRED, Scope_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Exp_SPAN), #2(SR_SPAN))
            in
              (UserCode.Scope_PROD_2_ACT (SR_RES, Exp_RES, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_case, _, strm') => Scope_PROD_2(strm)
          | (Tok.KW_if, _, strm') => Scope_PROD_2(strm)
          | (Tok.LP, _, strm') => Scope_PROD_2(strm)
          | (Tok.LCB, _, strm') => Scope_PROD_2(strm)
          | (Tok.MINUS, _, strm') => Scope_PROD_2(strm)
          | (Tok.DEREF, _, strm') => Scope_PROD_2(strm)
          | (Tok.UID(_), _, strm') => Scope_PROD_2(strm)
          | (Tok.LID(_), _, strm') => Scope_PROD_2(strm)
          | (Tok.NUMBER(_), _, strm') => Scope_PROD_2(strm)
          | (Tok.STRING(_), _, strm') => Scope_PROD_2(strm)
          | (Tok.KW_fun, _, strm') => Scope_PROD_1(strm)
          | (Tok.KW_let, _, strm') => Scope_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Type_NT (strm) = let
      val (TupleType_RES, TupleType_SPAN, strm') = TupleType_NT(strm)
      fun Type_PROD_1_SUBRULE_1_NT (strm) = let
            val (ARROW_RES, ARROW_SPAN, strm') = matchARROW(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(ARROW_SPAN), #2(Type_SPAN))
            in
              ((Type_RES), FULL_SPAN, strm')
            end
      fun Type_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ARROW, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Type_PROD_1_SUBRULE_1_PRED, Type_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(TupleType_SPAN), #2(SR_SPAN))
      in
        (UserCode.Type_PROD_1_ACT (SR_RES, TupleType_RES, SR_SPAN : (Lex.pos * Lex.pos), TupleType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and TupleType_NT (strm) = let
      val (AtomicType_RES, AtomicType_SPAN, strm') = AtomicType_NT(strm)
      fun TupleType_PROD_1_SUBRULE_1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (AtomicType_RES, AtomicType_SPAN, strm') = AtomicType_NT(strm')
            val FULL_SPAN = (#1(TIMES_SPAN), #2(AtomicType_SPAN))
            in
              ((AtomicType_RES), FULL_SPAN, strm')
            end
      fun TupleType_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TupleType_PROD_1_SUBRULE_1_PRED, TupleType_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(AtomicType_SPAN), #2(SR_SPAN))
      in
        (UserCode.TupleType_PROD_1_ACT (SR_RES, AtomicType_RES, SR_SPAN : (Lex.pos * Lex.pos), AtomicType_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and AtomicType_NT (strm) = let
      fun AtomicType_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.AtomicType_PROD_1_ACT (LP_RES, RP_RES, Type_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicType_PROD_2 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.AtomicType_PROD_2_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun AtomicType_PROD_3 (strm) = let
            val (UID_RES, UID_SPAN, strm') = matchUID(strm)
            fun AtomicType_PROD_3_SUBRULE_1_NT (strm) = let
                  val (TypeArgs_RES, TypeArgs_SPAN, strm') = TypeArgs_NT(strm)
                  val FULL_SPAN = (#1(TypeArgs_SPAN), #2(TypeArgs_SPAN))
                  in
                    ((TypeArgs_RES), FULL_SPAN, strm')
                  end
            fun AtomicType_PROD_3_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.LB, _, strm') => true
                    | _ => false
                  (* end case *))
            val (TypeArgs_RES, TypeArgs_SPAN, strm') = EBNF.optional(AtomicType_PROD_3_SUBRULE_1_PRED, AtomicType_PROD_3_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(UID_SPAN), #2(TypeArgs_SPAN))
            in
              (UserCode.AtomicType_PROD_3_ACT (TypeArgs_RES, UID_RES, TypeArgs_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.UID(_), _, strm') => AtomicType_PROD_3(strm)
          | (Tok.LP, _, strm') => AtomicType_PROD_1(strm)
          | (Tok.LID(_), _, strm') => AtomicType_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
and TypeArgs_NT (strm) = let
      val (LB_RES, LB_SPAN, strm') = matchLB(strm)
      val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
      fun TypeArgs_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Type_SPAN))
            in
              ((Type_RES), FULL_SPAN, strm')
            end
      fun TypeArgs_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TypeArgs_PROD_1_SUBRULE_1_PRED, TypeArgs_PROD_1_SUBRULE_1_NT, strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
      in
        (UserCode.TypeArgs_PROD_1_ACT (LB_RES, SR_RES, RB_RES, Type_RES, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), Type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ConDcl_NT (strm) = let
      val (UID_RES, UID_SPAN, strm') = matchUID(strm)
      fun ConDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm)
            val (Type_RES, Type_SPAN, strm') = Type_NT(strm')
            val FULL_SPAN = (#1(KW_of_SPAN), #2(Type_SPAN))
            in
              ((Type_RES), FULL_SPAN, strm')
            end
      fun ConDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_of, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(ConDcl_PROD_1_SUBRULE_1_PRED, ConDcl_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(UID_SPAN), #2(SR_SPAN))
      in
        (UserCode.ConDcl_PROD_1_ACT (SR_RES, UID_RES, SR_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TypeParams_NT (strm) = let
      val (LB_RES, LB_SPAN, strm') = matchLB(strm)
      val (LID_RES, LID_SPAN, strm') = matchLID(strm')
      fun TypeParams_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (LID_RES, LID_SPAN, strm') = matchLID(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(LID_SPAN))
            in
              ((LID_RES), FULL_SPAN, strm')
            end
      fun TypeParams_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(TypeParams_PROD_1_SUBRULE_1_PRED, TypeParams_PROD_1_SUBRULE_1_NT, strm')
      val (RB_RES, RB_SPAN, strm') = matchRB(strm')
      val FULL_SPAN = (#1(LB_SPAN), #2(RB_SPAN))
      in
        (UserCode.TypeParams_PROD_1_ACT (LB_RES, SR_RES, RB_RES, LID_RES, LB_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), RB_SPAN : (Lex.pos * Lex.pos), LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun DataDcl_NT (strm) = let
      val (KW_data_RES, KW_data_SPAN, strm') = matchKW_data(strm)
      val (UID_RES, UID_SPAN, strm') = matchUID(strm')
      fun DataDcl_PROD_1_SUBRULE_1_NT (strm) = let
            val (TypeParams_RES, TypeParams_SPAN, strm') = TypeParams_NT(strm)
            val FULL_SPAN = (#1(TypeParams_SPAN), #2(TypeParams_SPAN))
            in
              ((TypeParams_RES), FULL_SPAN, strm')
            end
      fun DataDcl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.LB, _, strm') => true
              | _ => false
            (* end case *))
      val (TypeParams_RES, TypeParams_SPAN, strm') = EBNF.optional(DataDcl_PROD_1_SUBRULE_1_PRED, DataDcl_PROD_1_SUBRULE_1_NT, strm')
      val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
      val (ConDcl_RES, ConDcl_SPAN, strm') = ConDcl_NT(strm')
      fun DataDcl_PROD_1_SUBRULE_2_NT (strm) = let
            val (BAR_RES, BAR_SPAN, strm') = matchBAR(strm)
            val (ConDcl_RES, ConDcl_SPAN, strm') = ConDcl_NT(strm')
            val FULL_SPAN = (#1(BAR_SPAN), #2(ConDcl_SPAN))
            in
              ((ConDcl_RES), FULL_SPAN, strm')
            end
      fun DataDcl_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.BAR, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(DataDcl_PROD_1_SUBRULE_2_PRED, DataDcl_PROD_1_SUBRULE_2_NT, strm')
      val FULL_SPAN = (#1(KW_data_SPAN), #2(SR_SPAN))
      in
        (UserCode.DataDcl_PROD_1_ACT (EQ_RES, SR_RES, TypeParams_RES, KW_data_RES, ConDcl_RES, UID_RES, EQ_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), TypeParams_SPAN : (Lex.pos * Lex.pos), KW_data_SPAN : (Lex.pos * Lex.pos), ConDcl_SPAN : (Lex.pos * Lex.pos), UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TopDcl_NT (strm) = let
      fun TopDcl_PROD_1 (strm) = let
            val (DataDcl_RES, DataDcl_SPAN, strm') = DataDcl_NT(strm)
            val FULL_SPAN = (#1(DataDcl_SPAN), #2(DataDcl_SPAN))
            in
              (UserCode.TopDcl_PROD_1_ACT (DataDcl_RES, DataDcl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TopDcl_PROD_2 (strm) = let
            val (ValBind_RES, ValBind_SPAN, strm') = ValBind_NT(strm)
            val FULL_SPAN = (#1(ValBind_SPAN), #2(ValBind_SPAN))
            in
              (UserCode.TopDcl_PROD_2_ACT (ValBind_RES, ValBind_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_fun, _, strm') => TopDcl_PROD_2(strm)
          | (Tok.KW_let, _, strm') => TopDcl_PROD_2(strm)
          | (Tok.KW_data, _, strm') => TopDcl_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Prog_NT (strm) = let
      fun Prog_PROD_1 (strm) = let
            val (Exp_RES, Exp_SPAN, strm') = Exp_NT(strm)
            fun Prog_PROD_1_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (Prog_RES, Prog_SPAN, strm') = Prog_NT(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(Prog_SPAN))
                  in
                    ((Prog_RES), FULL_SPAN, strm')
                  end
            fun Prog_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(Prog_PROD_1_SUBRULE_1_PRED, Prog_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Exp_SPAN), #2(SR_SPAN))
            in
              (UserCode.Prog_PROD_1_ACT (SR_RES, Exp_RES, SR_SPAN : (Lex.pos * Lex.pos), Exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Prog_PROD_2 (strm) = let
            val (TopDcl_RES, TopDcl_SPAN, strm') = TopDcl_NT(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val (Prog_RES, Prog_SPAN, strm') = Prog_NT(strm')
            val FULL_SPAN = (#1(TopDcl_SPAN), #2(Prog_SPAN))
            in
              (UserCode.Prog_PROD_2_ACT (TopDcl_RES, Prog_RES, SEMI_RES, TopDcl_SPAN : (Lex.pos * Lex.pos), Prog_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_data, _, strm') => Prog_PROD_2(strm)
          | (Tok.KW_fun, _, strm') => Prog_PROD_2(strm)
          | (Tok.KW_let, _, strm') => Prog_PROD_2(strm)
          | (Tok.KW_case, _, strm') => Prog_PROD_1(strm)
          | (Tok.KW_if, _, strm') => Prog_PROD_1(strm)
          | (Tok.LP, _, strm') => Prog_PROD_1(strm)
          | (Tok.LCB, _, strm') => Prog_PROD_1(strm)
          | (Tok.MINUS, _, strm') => Prog_PROD_1(strm)
          | (Tok.DEREF, _, strm') => Prog_PROD_1(strm)
          | (Tok.UID(_), _, strm') => Prog_PROD_1(strm)
          | (Tok.LID(_), _, strm') => Prog_PROD_1(strm)
          | (Tok.NUMBER(_), _, strm') => Prog_PROD_1(strm)
          | (Tok.STRING(_), _, strm') => Prog_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Program_NT (strm) = let
      val (Prog_RES, Prog_SPAN, strm') = Prog_NT(strm)
      val FULL_SPAN = (#1(Prog_SPAN), #2(Prog_SPAN))
      in
        (UserCode.Program_PROD_1_ACT (Prog_RES, Prog_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (Program_NT)
end
val Program_NT =  fn s => unwrap (Err.launch (eh, lexFn, Program_NT , true) s)

in (Program_NT) end
  in
fun parse lexFn  s = let val (Program_NT) = mk lexFn in Program_NT s end

  end

end
