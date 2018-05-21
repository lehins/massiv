	.file	"main.c"
	.section	.text.unlikely,"ax",@progbits
.LCOLDB0:
	.text
.LHOTB0:
	.p2align 4,,15
	.globl	add_i32_simple
	.type	add_i32_simple, @function
add_i32_simple:
.LFB0:
	.cfi_startproc
	xorl	%eax, %eax
	testl	%ecx, %ecx
	jle	.L1
	.p2align 4,,10
	.p2align 3
.L5:
	movl	(%rdx,%rax,4), %r8d
	addl	(%rsi,%rax,4), %r8d
	movl	%r8d, (%rdi,%rax,4)
	addq	$1, %rax
	cmpl	%eax, %ecx
	jg	.L5
.L1:
	rep ret
	.cfi_endproc
.LFE0:
	.size	add_i32_simple, .-add_i32_simple
	.section	.text.unlikely
.LCOLDE0:
	.text
.LHOTE0:
	.section	.text.unlikely
.LCOLDB1:
	.section	.text.startup,"ax",@progbits
.LHOTB1:
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB1:
	.cfi_startproc
	xorl	%eax, %eax
	ret
	.cfi_endproc
.LFE1:
	.size	main, .-main
	.section	.text.unlikely
.LCOLDE1:
	.section	.text.startup
.LHOTE1:
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.9) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
