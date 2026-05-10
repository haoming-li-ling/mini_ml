(* type-checker.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * The root of the ML Lite type checker.
 *)

structure TypeChecker : sig

    (* Type check a program and convert it to the typed AST representation.
     * Errors are reported on the given error stream.
     *)
    val check : Error.err_stream * BindTree.program -> AST.program

  end = struct

    structure BT  = BindTree
    structure C = Context
    structure Ty = Types
    structure TU = TypeUtil
    structure U = Unify

    (* typecheck a top-level declaration; if the declaration is a value binding,
     * then we return `SOME(lhs, rhs)`, otherwise we return `NONE`.
     *)
    fun chkDcl (cxt, BT.DclMark m) = chkDcl (C.withMark (cxt, m))
      | chkDcl (cxt, BT.DclData(tyc, tvs, cons)) = let
            val tvs' = List.map IdProps.tyvar tvs
            val tyC = TyCon.new (tyc, tvs')
            val Ty.DataTyc tyc' = tyC 
            val _ = C.addDataTyc (cxt, tyc') 
            (* check a list of constructors *)
            fun chkCons (cxt, []) = raise Fail "no cons!"
              | chkCons (cxt, c :: nil) = [chkCon (cxt, c)]
              | chkCons (cxt, c :: cs) = chkCon (cxt, c) :: chkCons (cxt, cs)
            (* check a single constructor *)
            and chkCon (cxt, BT.ConMark m) = chkCon (C.withMark (cxt, m))
              | chkCon (cxt, BT.Con (c, NONE)) = 
                  let
                    val tyvars = List.map (fn tv => Ty.TyVar tv) tvs'
                    val dsch = (tvs', Ty.TyCon (tyC, tyvars))
                    val dc = DataCon.new (tyC, c, NONE)
                    in 
                      IdProps.dconSet (c, dc); AST.C_DCON dc
                    end
              | chkCon (cxt, BT.Con (c, SOME t)) = 
                  let 
                    val tyvars = List.map (fn tv => Ty.TyVar tv) tvs'
                    val t' = ChkTy.check (cxt, t)
                    val dsch = (tvs', Ty.TyFun (t', Ty.TyCon (tyC, tyvars)))  
                    val dc = DataCon.new (tyC, c, SOME t')
                  in
                    IdProps.dconSet (c, dc); AST.C_DCON dc
                  end
              val _ = chkCons (cxt, List.rev cons)
          in
            NONE (* no value binding *)
          end
      | chkDcl (cxt, BT.DclVal bnd) = SOME (ChkExp.chkValBind(cxt, bnd))

    (* typecheck a program *)
    fun chkProg (cxt, BT.ProgMark m) = chkProg (C.withMark (cxt, m))
      | chkProg (cxt, BT.Prog (dcls, exp)) = let
          fun chkDcls [] = let
                val exp' = ChkExp.check (cxt, exp)
                in
                  (* the type of a program's expression must be Unit *)
                  if U.unify (Exp.typeOf exp', Basis.tyUnit)
                    then ()
                    else 
                      C.error (cxt, ["body of program must have type 'Unit'"]);
                  exp'
                end
            | chkDcls (dcl :: dclr) = (case chkDcl (cxt, dcl)
                 of SOME bind => Exp.mkLET (bind, chkDcls dclr)
                  | NONE => chkDcls dclr
                (* end case *))
          in
            chkDcls dcls
          end

  (* create the initial mapping from BindTree basis identifiers to AST identifiers *)
    fun initBasis () = (
          List.app IdProps.tyconSet [
	      (BindBasis.tycBool, Basis.tycBool),
	      (BindBasis.tycInt, Basis.tycInt),
	      (BindBasis.tycList, Basis.tycList),
	      (BindBasis.tycRef, Basis.tycRef),
	      (BindBasis.tycString, Basis.tycString),
	      (BindBasis.tycUnit, Basis.tycUnit)
            ];
          List.app IdProps.dconSet [
	      (BindBasis.conTrue, Basis.conTrue),
	      (BindBasis.conFalse, Basis.conFalse),
	      (BindBasis.conCons, Basis.conCons),
	      (BindBasis.conNil, Basis.conNil)
            ];
          List.app IdProps.varSet [
	      (BindBasis.opASSIGN, Basis.opASSIGN),
	      (BindBasis.opEQ, Basis.opEQ),
	      (BindBasis.opNEQ, Basis.opNEQ),
	      (BindBasis.opLTE, Basis.opLTE),
	      (BindBasis.opLT, Basis.opLT),
	      (BindBasis.opCONCAT, Basis.opCONCAT),
	      (BindBasis.opADD, Basis.opADD),
	      (BindBasis.opSUB, Basis.opSUB),
	      (BindBasis.opMUL, Basis.opMUL),
	      (BindBasis.opDIV, Basis.opDIV),
	      (BindBasis.opMOD, Basis.opMOD),
	      (BindBasis.opNEG, Basis.opNEG),
	      (BindBasis.opDEREF, Basis.opDEREF),
	      (BindBasis.varArguments, Basis.varArguments),
	      (BindBasis.varChr, Basis.varChr),
	      (BindBasis.varExit, Basis.varExit),
	      (BindBasis.varFail, Basis.varFail),
	      (BindBasis.varNewRef, Basis.varNewRef),
	      (BindBasis.varPrint, Basis.varPrint),
	      (BindBasis.varSize, Basis.varSize),
	      (BindBasis.varSub, Basis.varSub)
            ])

    fun check (errS, prog) = let
          val cxt = C.new errS
          val _ = initBasis ()
          val body = chkProg (cxt, prog)
          in
            AST.PROG {
                datatycs = C.getDataTycs cxt,
                body = body
              }
          end

  end
