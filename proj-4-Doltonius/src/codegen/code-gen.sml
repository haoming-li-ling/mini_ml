(* code-gen.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * The main code generation module.
 *)

structure CodeGen : sig

  (* `gen (base, prog)` generates LLVM assembly code into the file `base.ll` *)
    val gen : string * CFG.program -> unit

  end = struct

    structure P = Prim
    structure PC = PrimCond
    structure AG = ArithGen
    structure LF = LLVMFunc
    structure LB = LLVMBlock
    structure LV = LLVMVar
    structure LTy = LLVMType
    structure RT = MLLRuntime
    structure Info = CodeGenInfo

  (* the basic LLVM types that we use.  Note that when we are unsure
   * about the type of a ML Lite value, the we give it the `anyTy` so
   * that the GC will be aware of it.
   *)
    val anyTy = LTy.Ptr LTy.Int64
    val intTy = LTy.Int64

  (* convert an integer index to an LLVMVar.t representing it as a 32-bit integer constant *)
    fun index i = LV.const(LTy.Int32, IntInf.fromInt i)

    fun i64const n = LV.const (LTy.Int64, n)

  (* a flag for marking fragments that have already been visited *)
    local
      val {getFn, setFn} = CFGFrag.newFlag ()
    in
    val visited = getFn
    fun markVisited frag = setFn(frag, true)
    end (* local *)

  (* a property to associate a list of φ nodes with a join fragment *)
    val {setFn = setPhisForFrag, getFn = getPhisForFrag, ...} =
          CFGFrag.newProp (fn _ => ([] : LB.phi list))

  (* convert CFG values to LLVM *)
    fun cvtVal env v = (case v
           of CFG.VAR x => let
                val blk = Env.block env
                val v = Env.lookup(env, x)
                in
                  LB.emitCast(blk, Util.cvtType(CFGVar.typeOf x), v)
                end
            | CFG.INT n => i64const (AG.intToRep n)
            | CFG.STRING s => LV.global(Env.stringLit(env, s))
            | CFG.CODE lab => LV.global(Info.funcGlobal lab)
          (* end case *))

  (* `emitCall (env, ppt, f, args)` emits the function call `f (args)`, while
   * handling the bookkeeping related to managing the live variables at the
   * program point `ppt`.  It returns the pair `(ret, env)`, where `ret` is the
   * optional return value of the call and `env` is the updated environment.
   *)
    fun emitCall (env, ppt, f, args) = let
        (* get the live variables at this program point *)
          val live = Info.getLive ppt
        (* emit the call *)
          val {ret, live=live'} = LB.emitCall (Env.block env, {
                  func = f,
                  args = args,
                  live = List.map (fn x => Env.lookup(env, x)) live
                })
        (* rename the live variables in environment *)
          val env = ListPair.foldlEq
                (fn (x, x', env) => Env.bind(env, x, x'))
                  env (live, live')
          in
            case ret
             of SOME v => (v, env)
              | NONE => (i64const 1, env)
            (* end case *)
          end

  (* generate LLVM code for the given expression *)
    fun genExp (env, CFG.EXP(ppt, e)) = let
          val blk = Env.block env
          in
            case e
             of CFG.LET(x, rhs, e) => let
                  val _ = LB.emitComment(blk, concat[
                        "LET ", CFGVar.toString x, " = ", CFGUtil.rhsToString rhs
                      ])
                  val (result, env) = genRHS (env, blk, ppt, rhs)
                  in
                    genExp (Env.bind(env, x, result), e)
                  end
              | CFG.IF(tst, [v1, v2], jmp1, jmp2) => let
                  val test = (case tst 
                    of PC.IntLt => AG.lt (blk, env, v1, v2)
                     | PC.IntLte => AG.lte (blk, env, v1, v2)
                     | PC.IntEq => AG.equ (blk, env, v1, v2)
                     | PC.IntNEq => AG.neq (blk, env, v1, v2)
                     | PC.UIntLt => AG.ult (blk, env, v1, v2)
                     | _ => raise Fail "impossible")
                  in
                    LB.emitCondBr (blk, test, genJump (env, jmp1), genJump (env, jmp2))
                  end
              | CFG.TAIL_APPLY(appl as (ty, f, vs)) => let
                  val vs' = List.map (fn v => cvtVal env v) vs
                  in
                    LB.emitTailCall (blk, {func = cvtVal env f,args = vs'})
                  end
              | CFG.GOTO jmp => LB.emitBr (blk, genJump (env, jmp))
              | CFG.RETURN v => LB.emitReturn (blk, SOME(cvtVal env v))
            (* end case *)
          end

  (* generate LLVM code for the right-hand-side of a let binding.  This function returns
   * a pair of the result and environment (the environment is updated for calls, since
   * the mapping for live variables changes.
   *)
    and genRHS (env, blk, ppt, rhs) = let
          val blk = Env.block env
          in
            case rhs 
              of CFG.APPLY (appl as (ty, f, vs)) => emitCall (env, ppt, cvtVal env f, List.map (cvtVal env) vs)
               | CFG.CALL (r, vl) => emitCall (env, ppt, LV.global (RT.lookup r), 
                   List.map (cvtVal env) vl)
               | CFG.PRIM (p, vl) => let
                    val res = if P.arityOf p = 1
                    then
                      case (p, vl) 
                        of (P.IntNeg, [v]) => 
                             AG.sub (blk, env, CFG.INT (IntInf.fromInt 0), v)
                         | (P.StrSize, [v]) => LB.emitLd (blk, anyTy, cvtVal env v)
                         | (P.RefDeref, [v]) => 
                             LB.emitLd (blk, anyTy, cvtVal env v) 
                         | (P.RefNew, [v]) => let
                             val reg = LLVMReg.new anyTy
                             val vreg = LV.reg reg 
                             in 
                               LB.emitSt (blk, {addr = vreg, value = cvtVal env v});
                               vreg
                             end
                         | _ => raise Fail "impossible"
                    else
                      case (p, vl)
                        of (P.IntAdd, [v1, v2]) => AG.add (blk, env, v1, v2)
                         | (P.IntSub, [v1, v2]) => AG.sub (blk, env, v1, v2)
                         | (P.IntMul, [v1, v2]) => AG.mul (blk, env, v1, v2)
                         | (P.IntDiv, [v1, v2]) => AG.quot (blk, env, v1, v2)
                         | (P.IntMod, [v1, v2]) => AG.rem (blk, env, v1, v2)
                         | (P.StrSub, [v1, v2]) => LB.emitLd (blk, LTy.Int8, 
                             LB.emitAddr (blk, LTy.Int8, 
                               LB.emitAdd (blk, cvtVal env v1, i64const 1), 
                               cvtVal env v2))
                         | (P.RefAssign, [v1, v2]) => (
                             LB.emitSt (blk, 
                               {addr = cvtVal env v1, value = cvtVal env v2});
                             LV.const (LLVMType.Int1, 0))
                         | _ => raise Fail "impossible"
                    in
                      (res, env)
                    end
               | CFG.ALLOC vl => emitCall (env, ppt, 
                   LV.global RT.funAlloc, List.map (cvtVal env) vl) 
               | CFG.SEL (i, v) => (LB.emitLd (blk, anyTy, 
                   LB.emitAddr (blk, intTy, cvtVal env v, index i)), env)
          end

  (* handle a jump to another fragment/block; return the target label *)
    and genJump (env, (lab, args)) = (case CFGLabel.kindOf lab
          of CFGLabel.LK_Local frag => if visited frag 
            then let 
              val currentBlk = Env.block env
              val phis = getPhisForFrag frag
              val params' = List.map (cvtVal env) args
              fun defReg ([], []) = ()
                | defReg ((ph :: phs), (v :: vs)) = (
                    LB.addPhiDef (ph, v, currentBlk); 
                    defReg (phs, vs))
                | defReg (_, _) = raise Fail "impossible"
              in
                defReg (phis, params');
                LB.labelOf (Info.blockOf frag)
              end
            else let
              val currentBlk = Env.block env
              val blk = Info.blockOf frag
              val params = CFGFrag.paramsOf frag
              val params' = List.map (cvtVal env) args
              val env = Env.beginBlock (env, blk, params, params')
              in (
                if Info.isJoin frag
                  then let 
                    fun genReg [] = [] 
                      | genReg (v :: vs) = LB.emitPhi 
                          (blk, LLVMReg.newNamed (CFGVar.nameOf v, intTy)) ::
                          genReg vs
                    fun defReg ([], []) = ()
                      | defReg ((ph :: phs), (v :: vs)) = (
                          LB.addPhiDef (ph, v, currentBlk); 
                          defReg (phs, vs))
                      | defReg (_, _) = raise Fail "impossible"
                    val phis = genReg params 
                    in 
                      setPhisForFrag (frag, phis);
                      defReg (phis, params')
                    end
                  else
                    ());
                genExp (env, CFGFrag.bodyOf frag);
                markVisited frag;
                LLVMLabel.new (CFGLabel.nameOf lab)  
              end

            | _ => raise Fail "expected local label"
          (* end case *))

    fun genFunc env func = let
          val entryFrag = CFGFunct.entryOf func
        (* analyze the function and create its LLVM counter part *)
          val func' = Info.analyze (Env.moduleOf env, func)
          val entryBlk = LF.entryOf func'
        (* initialize the environment with the mapping from CFG parameters to LLVM *)
          val params = CFGFrag.paramsOf entryFrag
          val params' = List.map LV.reg (LF.paramsOf func')
          val env = Env.beginBlock (env, entryBlk, params, params')
          in
            genExp (env, CFGFrag.bodyOf entryFrag)
          end

    fun gen (baseName, CFG.PROG functions) = let
          val module = LLVMModule.new ()
          val env = Env.new module
          in
          (* generate the extern decls for runtime functions *)
            MLLRuntime.declareRuntimeGlobals module;
          (* generate code for functions *)
            List.app (genFunc env) functions;
          (* output code to file *)
            LLVMModule.output (baseName ^ ".ll", module)
          end

  end
