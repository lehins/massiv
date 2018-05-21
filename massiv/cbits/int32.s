	.file	"int32.c"
	.text
	.globl	add_i32
	.type	add_i32, @function
add_i32:
.LFB499:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -56(%rbp)
	movq	%rsi, -64(%rbp)
	movq	%rdx, -72(%rbp)
	movl	%ecx, -76(%rbp)
	movl	$0, -36(%rbp)
	jmp	.L2
.L4:
	movl	-36(%rbp), %eax
	cltq
	salq	$4, %rax
	movq	%rax, %rdx
	movq	-56(%rbp), %rax
	addq	%rdx, %rax
	movl	-36(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, %rcx
	salq	$4, %rcx
	movq	-72(%rbp), %rdx
	addq	%rcx, %rdx
	movdqa	(%rdx), %xmm0
	movl	-36(%rbp), %edx
	movslq	%edx, %rdx
	movq	%rdx, %rcx
	salq	$4, %rcx
	movq	-64(%rbp), %rdx
	addq	%rcx, %rdx
	movdqa	(%rdx), %xmm1
	movaps	%xmm1, -32(%rbp)
	movaps	%xmm0, -16(%rbp)
	movdqa	-32(%rbp), %xmm1
	movdqa	-16(%rbp), %xmm0
	paddd	%xmm1, %xmm0
	movaps	%xmm0, (%rax)
	addl	$1, -36(%rbp)
.L2:
	movl	-36(%rbp), %eax
	cmpl	-76(%rbp), %eax
	jl	.L4
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE499:
	.size	add_i32, .-add_i32
	.globl	add_i32_alt
	.type	add_i32_alt, @function
add_i32_alt:
.LFB500:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$24, %rsp
	movq	%rdi, -120(%rbp)
	movq	%rsi, -128(%rbp)
	movq	%rdx, -136(%rbp)
	movl	%ecx, -140(%rbp)
	movq	-120(%rbp), %rax
	movq	%rax, -96(%rbp)
	movq	-128(%rbp), %rax
	movq	%rax, -88(%rbp)
	movq	-136(%rbp), %rax
	movq	%rax, -80(%rbp)
	movl	$0, -100(%rbp)
	jmp	.L6
.L10:
	movl	-100(%rbp), %eax
	cltq
	salq	$4, %rax
	movq	%rax, %rdx
	movq	-80(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	movdqa	(%rax), %xmm0
	movl	-100(%rbp), %eax
	cltq
	salq	$4, %rax
	movq	%rax, %rdx
	movq	-88(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movdqa	(%rax), %xmm1
	movaps	%xmm1, -48(%rbp)
	movaps	%xmm0, -16(%rbp)
	movdqa	-48(%rbp), %xmm1
	movdqa	-16(%rbp), %xmm0
	paddd	%xmm1, %xmm0
	movl	-100(%rbp), %eax
	cltq
	salq	$4, %rax
	movq	%rax, %rdx
	movq	-96(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, -56(%rbp)
	movaps	%xmm0, -32(%rbp)
	movq	-56(%rbp), %rax
	movdqa	-32(%rbp), %xmm0
	movaps	%xmm0, (%rax)
	addl	$1, -100(%rbp)
.L6:
	movl	-100(%rbp), %eax
	cmpl	-140(%rbp), %eax
	jl	.L10
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE500:
	.size	add_i32_alt, .-add_i32_alt
	.globl	add_i32_simple
	.type	add_i32_simple, @function
add_i32_simple:
.LFB501:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movl	%ecx, -44(%rbp)
	movl	$0, -4(%rbp)
	jmp	.L12
.L13:
	movl	-4(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movq	-24(%rbp), %rax
	addq	%rdx, %rax
	movl	-4(%rbp), %edx
	movslq	%edx, %rdx
	leaq	0(,%rdx,4), %rcx
	movq	-32(%rbp), %rdx
	addq	%rcx, %rdx
	movl	(%rdx), %ecx
	movl	-4(%rbp), %edx
	movslq	%edx, %rdx
	leaq	0(,%rdx,4), %rsi
	movq	-40(%rbp), %rdx
	addq	%rsi, %rdx
	movl	(%rdx), %edx
	addl	%ecx, %edx
	movl	%edx, (%rax)
	addl	$1, -4(%rbp)
.L12:
	movl	-4(%rbp), %eax
	cmpl	-44(%rbp), %eax
	jl	.L13
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE501:
	.size	add_i32_simple, .-add_i32_simple
	.globl	add_i32_full
	.type	add_i32_full, @function
add_i32_full:
.LFB502:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$24, %rsp
	movq	%rdi, -120(%rbp)
	movq	%rsi, -128(%rbp)
	movq	%rdx, -136(%rbp)
	movl	%ecx, -140(%rbp)
	movl	$0, -100(%rbp)
	movq	-120(%rbp), %rax
	movq	%rax, -96(%rbp)
	movq	-128(%rbp), %rax
	movq	%rax, -88(%rbp)
	movq	-136(%rbp), %rax
	movq	%rax, -80(%rbp)
	jmp	.L15
.L19:
	movl	-100(%rbp), %eax
	cltq
	salq	$4, %rax
	movq	%rax, %rdx
	movq	-80(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	movdqa	(%rax), %xmm0
	movl	-100(%rbp), %eax
	cltq
	salq	$4, %rax
	movq	%rax, %rdx
	movq	-88(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movdqa	(%rax), %xmm1
	movaps	%xmm1, -48(%rbp)
	movaps	%xmm0, -16(%rbp)
	movdqa	-48(%rbp), %xmm1
	movdqa	-16(%rbp), %xmm0
	paddd	%xmm1, %xmm0
	movl	-100(%rbp), %eax
	cltq
	salq	$4, %rax
	movq	%rax, %rdx
	movq	-96(%rbp), %rax
	addq	%rdx, %rax
	movq	%rax, -56(%rbp)
	movaps	%xmm0, -32(%rbp)
	movq	-56(%rbp), %rax
	movdqa	-32(%rbp), %xmm0
	movaps	%xmm0, (%rax)
	addl	$1, -100(%rbp)
.L15:
	movl	-140(%rbp), %eax
	leal	3(%rax), %edx
	testl	%eax, %eax
	cmovs	%edx, %eax
	sarl	$2, %eax
	cmpl	-100(%rbp), %eax
	jg	.L19
	jmp	.L20
.L21:
	movl	-100(%rbp), %eax
	cltq
	leaq	0(,%rax,4), %rdx
	movq	-120(%rbp), %rax
	addq	%rdx, %rax
	movl	-100(%rbp), %edx
	movslq	%edx, %rdx
	leaq	0(,%rdx,4), %rcx
	movq	-128(%rbp), %rdx
	addq	%rcx, %rdx
	movl	(%rdx), %ecx
	movl	-100(%rbp), %edx
	movslq	%edx, %rdx
	leaq	0(,%rdx,4), %rsi
	movq	-136(%rbp), %rdx
	addq	%rsi, %rdx
	movl	(%rdx), %edx
	addl	%ecx, %edx
	movl	%edx, (%rax)
	addl	$1, -100(%rbp)
.L20:
	movl	-100(%rbp), %eax
	cmpl	-140(%rbp), %eax
	jl	.L21
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE502:
	.size	add_i32_full, .-add_i32_full
	.ident	"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.9) 5.4.0 20160609"
	.section	.note.GNU-stack,"",@progbits
