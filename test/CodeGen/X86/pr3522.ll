; RUN: llvm-as < %s | llc -march=x86 -stats |& not grep machine-sink
; PR3522

target triple = "i386-pc-linux-gnu"
@.str = external constant [13 x i8]		; <[13 x i8]*> [#uses=1]

define void @_ada_c34018a() {
entry:
	%0 = tail call i32 @report__ident_int(i32 90)		; <i32> [#uses=1]
	%1 = trunc i32 %0 to i8		; <i8> [#uses=1]
	invoke void @__gnat_rcheck_12(i8* getelementptr ([13 x i8]* @.str, i32 0, i32 0), i32 32) noreturn
			to label %invcont unwind label %lpad

invcont:		; preds = %entry
	unreachable

bb22:		; preds = %lpad
	ret void

return:		; preds = %lpad
	ret void

lpad:		; preds = %entry
	%2 = icmp eq i8 %1, 90		; <i1> [#uses=1]
	br i1 %2, label %return, label %bb22
}

declare void @__gnat_rcheck_12(i8*, i32) noreturn

declare i32 @report__ident_int(i32)
