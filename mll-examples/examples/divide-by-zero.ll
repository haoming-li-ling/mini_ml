define fastcc i64 @_mll_entry(i64 addrspace(1)* %arguments1) gc "statepoint-example" {
entry_0063:
	%r2 = icmp ne i64 1, 1
	br i1 %r2, label %then_0068, label %else_0069
else_0064:
	; LET _t00020062 = call _mll_fail ("Divide by zero")
	; emitCall: @_mll_fail (@_slit_0) ;; live = {}
	%r5 = bitcast [3 x i64] addrspace(1)* @_slit_0 to i64 addrspace(1)*
	%r6 = call token (i64,i32,void (i64 addrspace(1)*)*,i32,i32,...) @llvm.experimental.gc.statepoint.p0f_voidp1i64f(i64 0,i32 0,
		void (i64 addrspace(1)*)* @_mll_fail,
		i32 1,i32 0,
		i64 addrspace(1)* %r5,
		i32 0,i32 0)
	br label %join_0066
then_0065:
	; LET _t00010060 = IntDiv (42,0)
	%r3 = sdiv i64 42, 0
	br label %join_0067
join_0066:
	%_4 = phi i64 [ 1, %else_0064 ], [ %r3, %then_0065 ]
	ret i64 1
}


declare i64 addrspace(1)* @_mll_alloc(i64)
declare i64 addrspace(1)* @_mll_concat(i64 addrspace(1)*,i64 addrspace(1)*)
declare void @_mll_exit(i64)
declare void @_mll_fail(i64 addrspace(1)*)
declare void @_mll_print(i64 addrspace(1)*)
declare i64 addrspace(1)* @_mll_str_chr(i64)
@_slit_0 = global [3 x i64] [
	i64 29,
	i64 7070762896912181572,
	i64 122537119457401
]


declare token @llvm.experimental.gc.statepoint.p0f_voidp1i64f(i64,i32,void (i64 addrspace(1)*)*,i32,i32,...)
