(* chk-exp.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure ChkExp : sig

    (* convert a binding-tree expression to an AST expression, while checkinga
     * that it is well typed.s
     *)
    val check : Context.t * BindTree.exp -> AST.exp

    (* type check a value binding while converting it to AST *)
    val chkValBind : Context.t * BindTree.bind -> AST.bind

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Cov = Coverage
    structure Ty = Types
    structure TU = TypeUtil
    structure U = Unify
    structure B = Basis
    structure BB = BindBasis
    structure IP = IdProps

    (* an expression/type pair for when there is an error *)
    val bogusExp = AST.E(AST.E_TUPLE[], Ty.TyError)

    fun check (cxt, BT.ExpMark m) = check (C.withMark (cxt, m))
      | check (cxt, BT.ExpIf (e1, e2, e3)) =
          let 
            val e1a = check (cxt, e1)
            val AST.E (e1', t1) = e1a
          in 
            if U.unify (t1, B.tyBool) then 
              let
                val e2a = check (cxt, e2)
                val AST.E (e2', t2) = e2a
                val e3a = check (cxt, e3)
                val AST.E (e3', t3) = e3a
              in 
                if U.unify (t2, t3) then
                  Exp.mkCOND (e1a, e2a, e3a)
                else (
                  C.error (cxt, ["if statement unequal types"]);
                  bogusExp)
              end
            else (
              C.error (cxt, ["if condition not boolean"]);
              bogusExp)
         end
      | check (cxt, BT.ExpOrElse (e1, e2)) = 
          check (cxt, BT.ExpIf (e1, BT.ExpCon BB.conTrue, e2))
      | check (cxt, BT.ExpAndAlso (e1, e2)) = 
          check (cxt, BT.ExpIf (e1, e2, BT.ExpCon BB.conFalse))
      | check (cxt, BT.ExpBin (e1, v, e2)) = 
          check (cxt, BT.ExpApp (BT.ExpVar v, BT.ExpTuple [e1, e2]))
      | check (cxt, BT.ExpListCons (e1, e2)) = 
          check (cxt, BT.ExpApp (BT.ExpCon BB.conCons, BT.ExpTuple [e1, e2])) 
      | check (cxt, BT.ExpUn (v, e)) = check (cxt, BT.ExpApp (BT.ExpVar v, e)) 
      | check (cxt, BT.ExpApp (e1, e2)) = 
          let 
            val e1a = check (cxt, e1)
            val AST.E (e1', t1) = e1a
            val e2a = check (cxt, e2)
            val AST.E (e2', t2) = e2a
            val tr = TU.freshMV (C.depthOf cxt)
            val tf = Ty.TyFun (t2, tr)
          in 
            if U.unify (tf, t1)
              then AST.E (AST.E_APPLY (e1a, e2a), TU.prune tr)
              else (
                C.error (cxt, ["operand does not match with domain"]);
                bogusExp)
          end
      | check (cxt, BT.ExpVar v) = 
          let 
            val v' = IP.var v
            val (vs, t') = Var.typeOf v'
            val t = TU.instantiate (Var.typeOf v', C.depthOf cxt)
          in
            AST.E (AST.E_VAR v', t)
          end
      | check (cxt, BT.ExpCon c) = 
          let 
            val c' = IP.dcon c
            val t = TU.instantiate (DataCon.typeOf c', C.depthOf cxt)
          in
            AST.E (AST.E_CONST (AST.C_DCON c'), t)
          end
      | check (cxt, BT.ExpInt n) = Exp.mkINT n
      | check (cxt, BT.ExpStr s) = Exp.mkSTR s 
      | check (cxt, BT.ExpTuple es) = 
          Exp.mkTUPLE (List.map (fn e => check (cxt, e)) es)
      | check (cxt, BT.ExpCase (e, cs)) = 
          let 
            val ea = check (cxt, e)
            val AST.E (_, t) = ea
          in 
            case chkRules (cxt, t, cs)
              of SOME rs => Exp.mkCASE (ea, rs)
               | NONE => (
                  C.error (cxt, ["match redundant or non-exhaustive"]);
                  bogusExp)
          end
      | check (cxt, BT.ExpScope (bnds, e)) = chkScope (cxt, (bnds, e))
    (* typecheck a list of case-expression rules (including coverage checking) *)
    and chkRules (cxt, argTy, rules) = 
          let 
            (* check a single rule *)
            fun chkRule (cxt, argTy, BT.RuleMark r) =
                  let 
                    val (cxt', tree') = C.withMark (cxt, r)
                  in 
                    chkRule (cxt', argTy, tree')
                  end
              | chkRule (cxt, argTy, BT.RuleCase (p, sc)) = 
                  let 
                    val p' = ChkPat.check (cxt, argTy, p) 
                    val e' = chkScope (cxt, sc)
                  in
                    (p', e')
                  end
            val rules' = List.map (fn r => chkRule (cxt, argTy, r)) rules
            (* check if rules return same type *)
            fun compareRule (r, rs) = 
                  let 
                    val (_, AST.E (_, t)) = r
                    val (_, AST.E (_, ts)) = List.hd rs
                  in
                    not (U.unify (t, ts))
                  end
            val sameTy = List.filter (fn r => compareRule (r, rules')) rules'
          in   
            if List.null sameTy then
              let
                val at' = TU.prune argTy
                val cov = Cov.init at'
                (* compute coverage of a set of rules *)
                fun calcCov [] = raise Fail "no rules!"
                  | calcCov [r] = 
                      let 
                        val (p, _) = r
                      in
                        case Cov.update (cov, p)
                          of (_, true) => NONE
                           | (cov', false) => SOME cov'
                      end
                  | calcCov (r :: rs) = 
                      let 
                        val (p, _) = r
                      in
                        case calcCov rs
                          of SOME cov => (
                            case Cov.update (cov, p)
                              of (_, true) => NONE
                               | (cov', false) => SOME cov')
                           | NONE => NONE
                      end
                val cov = calcCov (List.rev rules')
              in 
                case cov 
                  of SOME cov' => 
                    if Cov.exhaustive cov' 
                      then SOME rules' 
                      else NONE
                   | NONE => NONE
              end
            else NONE
          end  


    (* typecheck a scope *)
    and chkScope (cxt, (bnds, e)) = let
          fun chk [] = check (cxt, e)
            | chk (bnd::bndr) = let
                val bnd' = chkValBind (cxt, bnd)
                in
                  Exp.mkLET (bnd', chk bndr)
                end
          in
            chk bnds
          end

    and chkValBind (cxt, BT.BindMark m) = chkValBind (C.withMark (cxt, m))
      | chkValBind (cxt, BT.BindFun (f, ps, e)) = 
          let
            (* check a list of patterns *)
            fun chkArgPats (cxt, []) = raise Fail "no arguments!"
              | chkArgPats (cxt, p :: nil) = 
                  let
                    val cxt' = C.incDepth cxt
                    val t = TU.freshMV (C.depthOf cxt')
                  in 
                    ([ChkPat.check (cxt', t, p)], [t], [cxt']) 
                  end
              | chkArgPats (cxt, p :: ps) = 
                  let
                    val cxt' = C.incDepth cxt
                    val t = TU.freshMV (C.depthOf cxt')
                    val p' = ChkPat.check (cxt', t, p)
                    val (ps', ts, cxts) = chkArgPats (cxt', ps)
                  in 
                    (p' :: ps', t :: ts, cxt' :: cxts)
                  end
            val (ps', ts, cxts) = chkArgPats (cxt, ps)
            val ec = List.last cxts
            val te = TU.freshMV (C.depthOf ec)
            val tf = List.foldr (fn (ft, rt) => Ty.TyFun (ft, rt)) te ts
            val vf = Var.new (f, tf)
            val _ = IdProps.varSet (f, vf)
            val ea = check (ec, e)
            val AST.E (_, et) = ea
          in
            if U.unify (et, te)
              then 
                let 
                  val tsch = TU.closeTy (tf, C.depthOf cxt)
                  val _ = Var.updateTy (vf, tsch)
                  val _ = IdProps.varSet (f, vf)
                in 
                  AST.B_FUN (vf, ps', ea)
                end
              else (
                C.error (cxt, ["function", 
                               BT.VarId.toString f, 
                               "return type does not match expression"]);
                AST.B_FUN (vf, ps', bogusExp))
          end
      | chkValBind (cxt, BT.BindVal (p, e)) = 
          let 
            val ea = check (cxt, e)
            val AST.E (_, t) = ea
            val p' = ChkPat.check (cxt, t, p)
          in 
            AST.B_VAL (p', ea)
          end
      | chkValBind (cxt, BT.BindExp e) = 
          let 
            val ea = check (cxt, e)
            val AST.E (_, t) = ea
          in 
            if U.unify (t, B.tyUnit)
              then AST.B_VAL (AST.P_TUPLE [], ea)
              else (
                C.error (cxt, ["expression not unit type"]); 
                AST.B_VAL (AST.P_TUPLE [], bogusExp))
          end
  end
