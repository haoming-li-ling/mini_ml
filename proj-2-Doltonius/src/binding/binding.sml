(* binding.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * Binding analysis for ML Lite.
 *)

structure Binding : sig

    (* check the bindings in a ML Lite parse-tree and return a binding-tree
     * representation that makes the connection between binding and use
     * occurrences of identifiers explicit.
     *)
    val analyze : Error.err_stream * ParseTree.program -> BindTree.program

  end = struct

    structure PT = ParseTree
    structure BT = BindTree
    structure BB = BindBasis
    structure C = Context

    (* dummy binding-trees that we can use when an error prevents us from
     * constructing an actual tree.
     *)
    val bogusTy = BT.TyTuple[]
    val bogusExp = BT.ExpTuple[]
    val bogusPat = BT.PatWild

    (* The following two helper functions are used to process the mark nodes
     * in the parse tree.
     *
     * `chkWithMark wrap chk (cxt, {span, tree})` applies the `chk` function
     * to `tree` using a context that has been updated with the `span`.  The
     * resulting bind-tree form is then paired with span and wrapped by the
     * bind-tree constructor `wrap`.
     *)
    fun chkWithMark wrap chk (cxt, {span, tree}) =
          wrap {span = span, tree = chk (C.setSpan(cxt, span), tree)}

    (* `chkWithMark'` is similar to `chkWithMark`, except that it handles
     * `chk` functions that return an extended context.
     *)
    fun chkWithMark' wrap chk (cxt, {span, tree}) = let
          val (tree', cxt') = chk (C.setSpan(cxt, span), tree)
          in
            (wrap {span = span, tree = tree'}, cxt')
          end

    (* converts a list of ids to a list of BT.TyVar *)
    fun tvList [] = []
      | tvList (t :: ts) = let 
            val newt = BT.TyVar.new t 
          in newt :: tvList ts 
          end

    (* performs binding analysis on program *)
    fun analyze (errS, prog) = let
          (* report an unbound-identifier error *)
          fun unbound (cxt, kind, id) =
                Error.errorAt(errS, C.spanOf cxt, [
                    "unbound ", kind, " `", Atom.toString id, "`"
                  ])
          (* report a duplicate identifier error; the second argument specifies
           * the kind of identifier as a string.
           *)
          fun duplicate (cxt, kind, x) = Error.errorAt (errS, C.spanOf cxt, [
                  "duplicate ", kind, " `", Atom.toString x, "` "
                ])
          (* analyze a program *)
          fun chkProg (cxt, PT.ProgMark m) = chkWithMark 
                BT.ProgMark chkProg (cxt, m)
            | chkProg (cxt, PT.Prog(dcls, exp)) = 
                (* process each of the top-level declarations 
                 * while accumulating their
                 * bindings in the context.
                 *)
                let
                  fun chkDcls (cxt, [], dcls') = BT.Prog 
                        (List.rev dcls', chkExp(cxt, exp))
                    | chkDcls (cxt, dcl::dcls, dcls') = let
                          val (dcl', cxt) = chkDcl (cxt, dcl)
                        in
                          chkDcls (cxt, dcls, dcl'::dcls')
                        end
                in
                  chkDcls (cxt, dcls, [])
                end
          (* analyze a declaration *)
          and chkDcl (cxt, PT.DclMark m) = chkWithMark' 
                BT.DclMark chkDcl (cxt, m)
            | chkDcl (cxt, PT.DclData (t, tl, [])) = let 
                  val newt = BT.TycId.new t
                  val cxt_tc' = C.bindTyCon (cxt, t, newt)
                  val tv = tvList tl
                in
                  (BT.DclData (newt, tv, []), cxt_tc')
                end
            | chkDcl (cxt, PT.DclData (t, tl, c :: cs)) = let
                  val tvlist = tvList tl
                  val tv = toMap (cxt, tvlist)
                  val newt = BT.TycId.new t
                  val cxt_tc' = C.bindTyCon (cxt, t, newt)
                  val g = C.clearConEnv (C.setTVEnv (cxt_tc', tv))
                  fun chkConList (cxt, []) = ([], [])
                    | chkConList (cxt, c :: cs) = let
                          val (c1 :: cs1) =  (c :: cs)
                          val (c', cxt') = chkCon (cxt, c1)
                          val (cs', cxts') = chkConList (cxt', cs1)
                        in 
                          ((c' :: cs'), (cxt' :: cxts'))
                        end
                  val (conlist, cxtlist) = (chkConList (g, c :: cs))
                  val cxtm = List.last cxtlist 
                  val cxtd = C.mergeConEnv (cxt_tc', C.getConEnv cxtm)
                in
                  (BT.DclData (newt, tvlist, conlist), cxtd)
                end
            | chkDcl (cxt, PT.DclVal b) = let
                    val (dcl', v'') = chkBind (cxt, b)
                    val v' = C.getVarEnv v''
                    val cxt'' = C.mergeVarEnv (cxt, v')
                  in
                    (BT.DclVal dcl', cxt'')
                  end
            (* converts a list of BT.TyVar items to a BT.TyVar AtomMap *)
          and toMap (cxt, []) = AtomMap.empty
            | toMap (cxt, i :: il) = let 
                  val (i' :: il') = List.rev (i :: il)
                  val ilmap' = toMap (cxt, il')
                  val cxt' = C.setTVEnv (cxt, ilmap')
                  val iname' = Atom.atom (BT.TyVar.nameOf i')
                in 
                 (case C.findTyVar (cxt', iname') 
                    of SOME ii => (duplicate (cxt', "type variable", iname');
                                   AtomMap.insert (ilmap', iname', ii))
                     | NONE => (AtomMap.insert (ilmap', iname', i')))
                end
          (* analyze a constructor declaration *)
          and chkCon (cxt, PT.ConMark m) = chkWithMark' 
                BT.ConMark chkCon (cxt, m)
            | chkCon (cxt, PT.Con (c, SOME t)) = (
                case C.findCon (cxt, c) 
                  of SOME c' => (duplicate (cxt, "data constructor", c);
                                 (BT.Con (c', SOME (chkTy (cxt, t))), cxt)) 
                   | NONE => let 
                      val newc = BT.ConId.new c
                      val cxt' = C.bindCon (cxt, c, newc)
                    in 
                      (BT.Con (newc, SOME (chkTy (cxt, t))), cxt')
                    end)
            | chkCon (cxt, PT.Con (c, NONE)) = (
                case C.findCon (cxt, c) 
                  of SOME c' => (duplicate (cxt, "data constructor", c);
                                 (BT.Con (c', NONE), cxt)) 
                   | NONE => let 
                      val newc = BT.ConId.new c
                      val cxt' = C.bindCon (cxt, c, newc)
                    in 
                      (BT.Con (newc, NONE), cxt')
                    end)
          (* analyze types *)
          and chkTy (cxt, PT.TyMark m) = chkWithMark BT.TyMark chkTy (cxt, m)
            | chkTy (cxt, PT.TyVar v) = (
                case C.findTyVar (cxt, v) 
                  of SOME v' => BT.TyVar v'
                   | NONE => (unbound (cxt, "type variable", v); bogusTy)) 
            | chkTy (cxt, PT.TyCon (c, [])) = (
                case C.findTyCon (cxt, c)
                  of SOME c' => BT.TyCon (c', [])
                   | NONE => (unbound (cxt, "type constructor", c); bogusTy)) 
            | chkTy (cxt, PT.TyCon (c, tl)) = (
                case C.findTyCon (cxt, c)
                  of SOME c' => let 
                      fun tyList (cxt, []) = []
                        | tyList (cxt, t :: ts) = 
                            chkTy (cxt, t) :: tyList (cxt, ts)
                      val tl' = tyList (cxt, tl)
                    in 
                      BT.TyCon (c', tl')
                    end
                   | NONE => (unbound (cxt, "type constructor", c); bogusTy)) 
            | chkTy (cxt, PT.TyFun (t1, t2)) = BT.TyFun 
                (chkTy (cxt, t1), chkTy (cxt, t2))
            | chkTy (cxt, PT.TyTuple tl) = let
                  fun tyList (cxt, []) = []
                        | tyList (cxt, t :: ts) = 
                            chkTy (cxt, t) :: tyList (cxt, ts)
                  val tl' = tyList (cxt, tl)
                in 
                  BT.TyTuple tl'
                end 
          (* analyze binding structures *)
          and chkBind (cxt, PT.BindMark m) = chkWithMark' 
                BT.BindMark chkBind (cxt, m)
            | chkBind (cxt, PT.BindFun (f, [], e)) = let 
                  val newf = BT.VarId.new f
                  val cxt' = C.bindVar (cxt, f, newf)
                in 
                  (BT.BindFun (newf, [], chkExp (cxt', e)), cxt')
                end
            | chkBind (cxt, PT.BindFun (f, p :: ps, e)) = let 
                  val newf = BT.VarId.new f
                  val cxt' = C.bindVar (cxt, f, newf)
                  val cxt_nv = C.clearVarEnv cxt
                  val (BT.PatTuple pl, cxt_nvn) = chkPat 
                        (cxt_nv, PT.PatTuple (p :: ps))
                  val cxt_e = C.mergeVarEnv (cxt', C.getVarEnv cxt_nvn)
                in 
                  (BT.BindFun (newf, pl, chkExp (cxt_e, e)), cxt')
                end
            | chkBind (cxt, PT.BindVal (p, e)) = let
                  val cxt_nv = C.clearVarEnv cxt
                  val (p', cxt') = chkPat (cxt_nv, p)
                  val cxt_p = C.mergeVarEnv (cxt, C.getVarEnv cxt')
                in 
                  (BT.BindVal (p', chkExp (cxt, e)), cxt_p)
                end
            | chkBind (cxt, PT.BindExp e1) = 
                (BT.BindExp (chkExp (cxt, e1)), cxt)
          (* analyze expresssions *)
          and chkExp (cxt, PT.ExpMark m) = chkWithMark 
                BT.ExpMark chkExp (cxt, m)
            | chkExp (cxt, PT.ExpIf (e1, e2, e3)) = BT.ExpIf 
                (chkExp (cxt, e1), chkExp (cxt, e2), chkExp (cxt, e3))
            | chkExp (cxt, PT.ExpOrElse (e1, e2)) = BT.ExpOrElse 
                (chkExp (cxt, e1), chkExp (cxt, e2))
            | chkExp (cxt, PT.ExpAndAlso (e1, e2)) = BT.ExpAndAlso 
                (chkExp (cxt, e1), chkExp (cxt, e2)) 
            | chkExp (cxt, PT.ExpBin (e1, i, e2)) = BT.ExpBin 
                (chkExp (cxt, e1), C.lookupOp (cxt, i), chkExp (cxt, e2))
            | chkExp (cxt, PT.ExpListCons (e1, e2)) = BT.ExpListCons 
                (chkExp (cxt, e1), chkExp (cxt, e2))
            | chkExp (cxt, PT.ExpUn (i, e1)) = BT.ExpUn 
                (valOf (C.findVar (cxt, i)), chkExp (cxt, e1))
            | chkExp (cxt, PT.ExpApp (e1, e2)) = BT.ExpApp 
                (chkExp (cxt, e1), chkExp (cxt, e2))
            | chkExp (cxt, PT.ExpVar i) = (
                case C.findVar (cxt, i) 
                  of SOME v => BT.ExpVar v 
                   | NONE => (unbound (cxt, "variable", i); bogusExp))
            | chkExp (cxt, PT.ExpCon i) = (
                case C.findCon (cxt, i) 
                  of SOME c => BT.ExpCon c 
                   | NONE => (unbound (cxt, "unbound constructor", i);
                              bogusExp))
            | chkExp (cxt, PT.ExpInt n) = BT.ExpInt n
            | chkExp (cxt, PT.ExpStr s) = BT.ExpStr s
            | chkExp (cxt, PT.ExpTuple (e :: es)) = 
                BT.ExpTuple ((chkExp (cxt, e)) :: (
                  case chkExp (cxt, PT.ExpTuple es) 
                    of BT.ExpTuple es' => es' 
                     | _ => raise Fail "wrong syntax" ))
            | chkExp (cxt, PT.ExpTuple []) = BT.ExpTuple []
            | chkExp (cxt, PT.ExpCase (e, r :: rs)) = 
                BT.ExpCase (chkExp (cxt, e), chkRule (cxt, r) :: (
                  case chkExp (cxt, PT.ExpCase (e, rs)) 
                    of BT.ExpCase (_, rs') => rs' 
                     | _ => raise Fail "wrong syntax"))
            | chkExp (cxt, PT.ExpCase (e, [])) = BT.ExpCase 
                (chkExp (cxt, e), [])
            | chkExp (cxt, PT.ExpScope sc) = BT.ExpScope (chkScope (cxt, sc)) 
          (* analyze rules *)
          and chkRule (cxt, PT.RuleMark m) = chkWithMark 
                BT.RuleMark chkRule (cxt, m)
            | chkRule (cxt, PT.RuleCase (p, sc)) = let
                  val cxt_nv = C.clearVarEnv cxt
                  val (p', cxt') = chkPat (cxt_nv, p)
                  val v' = C.getVarEnv cxt'
                  val cxt_v' = C.mergeVarEnv (cxt, v')
                in 
                  BT.RuleCase (p', chkScope (cxt_v', sc))
                end
          (* analyze scopes *)
          and chkScope (cxt, ([], e)) = ([], chkExp (cxt, e))
            | chkScope (cxt, ((b :: bs), e)) = let 
                  val (b', cxt') = chkBind (cxt, b)
                  val (bl, e') = chkScope (cxt', (bs, e))
                in 
                  (b' :: bl, e')
                end
          (* analyze patterns *)
          and chkPat (cxt, PT.PatMark m) = chkWithMark' 
                BT.PatMark chkPat (cxt, m)
            | chkPat (cxt, PT.PatVar i) = (
                case C.findVar (cxt, i) 
                  of SOME _ => 
                      (duplicate (cxt, "pattern variable", i); 
                       (bogusPat, cxt)) 
                   | NONE => let 
                        val newVar = BT.VarId.new i 
                      in
                        (BT.PatVar newVar, (C.bindVar (cxt, i, newVar)))
                      end)
            | chkPat (cxt, PT.PatCon (i, NONE)) = (
                case C.findCon (cxt, i) 
                  of SOME c => (BT.PatCon (c, NONE), cxt) 
                   | NONE => 
                      (unbound (cxt, "constructor", i); 
                      (bogusPat, cxt)))
            | chkPat (cxt, PT.PatCon (i, SOME p)) = (
                case C.findCon (cxt, i) 
                  of SOME c => let 
                        val (p', cxt') = chkPat (cxt, p)
                      in 
                        (BT.PatCon (c, SOME p'), cxt')
                      end
                   | NONE => 
                      (unbound (cxt, "constructor", i); 
                      (bogusPat, cxt)))
            | chkPat (cxt, PT.PatListCons (p1, p2)) = let 
                  val (p1', cxt1') = chkPat (cxt, p1)
                  val (p2', cxt2') = chkPat (cxt1', p2)
                in 
                  (BT.PatListCons (p1', p2'), cxt2')
                end
            | chkPat (cxt, PT.PatTuple []) = (BT.PatTuple [], cxt)
            | chkPat (cxt, PT.PatTuple (p :: ps)) = let 
                  val (p', cxt') = chkPat (cxt, p)
                  val (BT.PatTuple ps', cxts') = chkPat (cxt', PT.PatTuple ps)
                in 
                  (BT.PatTuple (p' :: ps'), cxts')
                end
            | chkPat (cxt, PT.PatWild) = (BT.PatWild, cxt)
          in
            chkProg (C.new errS, prog)
          end (* analyze *)

  end (* Binding *)
