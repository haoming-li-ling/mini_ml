(* chk-pat.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *)

structure ChkPat : sig

    (* type check a pattern with respect to the given type.  Return the AST
     * equivalent pattern.
     * Note: the specified type of the pattern might just be a meta variable
     * (e.g., when the pattern is a function parameter), but when it has more
     * information we can check that against the structure of the pattern.
     *)
    val check : Context.t * Types.ty * BindTree.pat -> AST.pat

  end = struct

    structure BT = BindTree
    structure C = Context
    structure Ty = Types
    structure TU = TypeUtil
    structure U = Unify
    structure DC = DataCon
    structure BB = BindBasis

    (* placeholder pattern for when there is an error *)
    val bogusPat = AST.P_TUPLE[]

    fun check (cxt, t, BT.PatMark m) = 
          let 
            val (cxt', m') = C.withMark (cxt, m)
          in
            check (cxt', t, m')
          end
      | check (cxt, t, BT.PatVar v) = 
          let
            val v' = Var.new (v, t)
          in
            (IdProps.varSet (v, v'); AST.P_VAR v') 
          end
      | check (cxt, t, BT.PatCon (c, NONE)) = 
          let 
            val dc = IdProps.dcon c
            val tc = DC.typeOf dc
            val itc = TU.instantiate (tc, C.depthOf cxt)
          in
            if U.unify (itc, t) 
              then AST.P_CONST dc
              else (
                C.error (cxt, ["data constructor ", 
                               BT.ConId.toString c, 
                               "untypable in pattern"]); 
                bogusPat)
          end
      | check (cxt, t, BT.PatCon (c, SOME p)) = 
          let 
            val dc = IdProps.dcon c
            val tc = DC.typeOf dc
            val (tvs, ty) = tc
            val itc = TU.instantiate (tc, C.depthOf cxt)
            val t' = TU.freshMV (C.depthOf cxt)
          in
            if U.unify (itc, Ty.TyFun (t', t))
              then AST.P_CON (dc, check (cxt, t', p)) 
              else (
                C.error (cxt, ["data constructor ", 
                               BT.ConId.toString c, 
                               "untypable in pattern"]); 
                bogusPat)
          end
      | check (cxt, t, BT.PatListCons (p1, p2)) = 
          check (cxt, t, BT.PatCon (BB.conCons, SOME (BT.PatTuple [p1, p2])))
      | check (cxt, t, BT.PatTuple []) = 
          if U.unify (t, Basis.tyUnit) 
            then AST.P_TUPLE []
            else (
              C.error (cxt, ["() assigned type other than Unit"]); 
              bogusPat)
      | check (cxt, t, BT.PatTuple [_]) = raise Fail "1-tuple impossible!"
      | check (cxt, t, BT.PatTuple (p :: ps)) = 
          let 
            (* check a list of patterns *)
            fun chkPats [p] = 
                  let 
                    val mt = TU.freshMV (C.depthOf cxt)
                    val p' = check (cxt, mt, p)
                  in
                    ([p'], [mt])
                  end
              | chkPats (p :: ps) = 
                  let 
                    val mt = TU.freshMV (C.depthOf cxt)
                    val p' = check (cxt, mt, p)
                    val (ps', mts) = chkPats ps
                  in 
                    (p' :: ps', mt :: mts)
                  end
              | chkPats [] = raise Fail "0-tuple other than () impossible!"
            val (ps', mts) = chkPats (p :: ps)
            val ttup = Ty.TyTuple mts 
          in
            if U.unify (ttup, t)
              then AST.P_TUPLE ps'
              else (
                C.error (cxt, ["Unable to type tuple pattern with assigned type"]);
                bogusPat)
          end
      | check (cxt, t, BT.PatWild) = AST.P_VAR (Var.wild t)


  end
