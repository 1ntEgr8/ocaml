/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2003 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, AMD64 processor */
/* Must be preprocessed by cpp */

/* PIC mode support based on contribution by Paul Stravers (see PR#4795) */

#include "caml/m.h"

#if defined(SYS_macosx)

#define LBL(x) L##x
#define G(r) _##r
#define GREL(r) _##r@GOTPCREL
#define GCALL(r) _##r
#define TEXT_SECTION(name) .text
#define FUNCTION_ALIGN 2
#define EIGHT_ALIGN 3
#define SIXTEEN_ALIGN 4
#define FUNCTION(name) \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#elif defined(SYS_mingw64) || defined(SYS_cygwin)

#define LBL(x) .L##x
#define G(r) r
#undef  GREL
#define GCALL(r) r
#define TEXT_SECTION(name)
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .align FUNCTION_ALIGN; \
        name:

#else

#define LBL(x) .L##x
#define G(r) r
#define GREL(r) r@GOTPCREL
#define GCALL(r) r@PLT
#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif
#define FUNCTION_ALIGN 4
#define EIGHT_ALIGN 8
#define SIXTEEN_ALIGN 16
#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .globl name; \
        .type name,@function; \
        .align FUNCTION_ALIGN; \
        name:

#endif

#if defined(SYS_linux) || defined(SYS_gnu)
#define ENDFUNCTION(name) \
        .size name, . - name
#else
#define ENDFUNCTION(name)
#endif

#ifdef ASM_CFI_SUPPORTED
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_OFFSET(r, n) .cfi_offset r, n
#define CFI_SAME_VALUE(r) .cfi_same_value r
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_OFFSET(r, n)
#define CFI_SAME_VALUE(r)
#endif

#ifdef WITH_FRAME_POINTERS

#define ENTER_FUNCTION \
        pushq   %rbp; CFI_ADJUST(8); \
        movq    %rsp, %rbp
#define LEAVE_FUNCTION \
        popq    %rbp; CFI_ADJUST(-8);

#else

#define ENTER_FUNCTION \
        subq    $8, %rsp; CFI_ADJUST (8);
#define LEAVE_FUNCTION \
        addq    $8, %rsp; CFI_ADJUST (-8);

#endif

        .set    domain_curr_field, 0
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#include "../runtime/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) (8*domain_field_caml_##var)(%r14)

#if defined(__PIC__) && !defined(SYS_mingw64) && !defined(SYS_cygwin)

/* Position-independent operations on global variables. */

/* Store [srcreg] in global [dstlabel].  Clobbers %r11. */
#define STORE_VAR(srcreg,dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        movq    srcreg, (%r11)

#define STORE_VAR32(srcreg,dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        movl    srcreg, (%r11)

/* Load global [srclabel] in register [dstreg].  Clobbers %r11. */
#define LOAD_VAR(srclabel,dstreg) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        movq    (%r11), dstreg

/* Compare global [label] with register [reg].  Clobbers %rax. */
#define CMP_VAR(label,reg) \
        movq    GREL(label)(%rip), %rax ; \
        cmpq    (%rax), reg

/* Test 32-bit global [label] against mask [imm].  Clobbers %r11. */
#define TESTL_VAR(imm,label) \
        movq    GREL(label)(%rip), %r11 ; \
        testl   imm, (%r11)

/* Push global [label] on stack.  Clobbers %r11. */
#define PUSH_VAR(srclabel) \
        movq    GREL(srclabel)(%rip), %r11 ; \
        pushq   (%r11); CFI_ADJUST (8)

/* Pop global [label] off stack.  Clobbers %r11. */
#define POP_VAR(dstlabel) \
        movq    GREL(dstlabel)(%rip), %r11 ; \
        popq    (%r11);  CFI_ADJUST (-8)

/* Load address of global [label] in register [dst]. */
#define LEA_VAR(label,dst) \
        movq    GREL(label)(%rip), dst

#else

/* Non-PIC operations on global variables.  Slightly faster. */

#define STORE_VAR(srcreg,dstlabel) \
        movq    srcreg, G(dstlabel)(%rip)

#define STORE_VAR32(srcreg,dstlabel) \
        movl    srcreg, G(dstlabel)(%rip)

#define LOAD_VAR(srclabel,dstreg) \
        movq    G(srclabel)(%rip), dstreg

#define CMP_VAR(label,reg) \
        cmpq    G(label)(%rip), %r15

#define TESTL_VAR(imm,label) \
        testl   imm, G(label)(%rip)

#define PUSH_VAR(srclabel) \
        pushq   G(srclabel)(%rip) ; CFI_ADJUST(8)

#define POP_VAR(dstlabel) \
        popq    G(dstlabel)(%rip); CFI_ADJUST(-8)

#define LEA_VAR(label,dst) \
        leaq    G(label)(%rip), dst
#endif

/* Save and restore all callee-save registers on stack.
   Keep the stack 16-aligned. 
   Note: the CALLEE_SAVE_REGS macros are 8-byte unaligned as it is
   assumed to be applied right on function entry/exit (when (esp+8) is 16-byte aligned)
*/

#if defined(SYS_mingw64) || defined(SYS_cygwin)

/* Win64 API: callee-save regs are rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST (8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST (8); CFI_OFFSET(rbp, -24); \
                      /* Allows debugger to walk the stack */ \
        pushq   %rsi; CFI_ADJUST (8); CFI_OFFSET(rsi, -32); \
        pushq   %rdi; CFI_ADJUST (8); CFI_OFFSET(rdi, -40); \
        pushq   %r12; CFI_ADJUST (8); CFI_OFFSET(r12, -48); \
        pushq   %r13; CFI_ADJUST (8); CFI_OFFSET(r13, -56); \
        pushq   %r14; CFI_ADJUST (8); CFI_OFFSET(r14, -64); \
        pushq   %r15; CFI_ADJUST (8); CFI_OFFSET(r15, -72); \
        subq    $(8+10*16), %rsp; CFI_ADJUST (8+10*16); \
        movupd  %xmm6, 0*16(%rsp); \
        movupd  %xmm7, 1*16(%rsp); \
        movupd  %xmm8, 2*16(%rsp); \
        movupd  %xmm9, 3*16(%rsp); \
        movupd  %xmm10, 4*16(%rsp); \
        movupd  %xmm11, 5*16(%rsp); \
        movupd  %xmm12, 6*16(%rsp); \
        movupd  %xmm13, 7*16(%rsp); \
        movupd  %xmm14, 8*16(%rsp); \
        movupd  %xmm15, 9*16(%rsp);

#define POP_CALLEE_SAVE_REGS \
        movupd  0*16(%rsp), %xmm6; \
        movupd  1*16(%rsp), %xmm7; \
        movupd  2*16(%rsp), %xmm8; \
        movupd  3*16(%rsp), %xmm9; \
        movupd  4*16(%rsp), %xmm10; \
        movupd  5*16(%rsp), %xmm11; \
        movupd  6*16(%rsp), %xmm12; \
        movupd  7*16(%rsp), %xmm13; \
        movupd  8*16(%rsp), %xmm14; \
        movupd  9*16(%rsp), %xmm15; \
        addq    $(8+10*16), %rsp; CFI_ADJUST (-8-10*16); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rdi; CFI_ADJUST(-8); CFI_SAME_VALUE(rdi); \
        popq    %rsi; CFI_ADJUST(-8); CFI_SAME_VALUE(rsi); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#define PUSH_CALLERSAVE_INT_BASE_REGS_UNALIGNED \
        pushq   %rax; CFI_ADJUST (8);  \
        pushq   %rcx; CFI_ADJUST (8);  \
        pushq   %rdx; CFI_ADJUST (8);  \
        pushq   %r8; CFI_ADJUST (8);   \
        pushq   %r9; CFI_ADJUST (8);

#define POP_CALLERSAVE_INT_BASE_REGS_UNALIGNED \
        popq    %r9; CFI_ADJUST (-8); \
        popq    %r8; CFI_ADJUST (-8); \
        popq    %rdx; CFI_ADJUST (-8); \
        popq    %rcx; CFI_ADJUST (-8); \
        popq    %rax; CFI_ADJUST (-8);

#define PUSH_CALLERSAVE_FP_REGS \
        subq    $(16*6), %rsp; CFI_ADJUST (16*6); \
        movaps  %xmm0, 0*16(%rsp); \
        movaps  %xmm1, 1*16(%rsp); \
        movaps  %xmm2, 2*16(%rsp); \
        movaps  %xmm3, 3*16(%rsp); \
        movaps  %xmm4, 4*16(%rsp); \
        movaps  %xmm5, 5*16(%rsp);

#define POP_CALLERSAVE_FP_REGS \
        movaps  0*16(%rsp), %xmm0; \
        movaps  1*16(%rsp), %xmm1; \
        movaps  2*16(%rsp), %xmm2; \
        movaps  3*16(%rsp), %xmm3; \
        movaps  4*16(%rsp), %xmm4; \
        movaps  5*16(%rsp), %xmm5; \
        addq    $(16*6), %rsp; CFI_ADJUST(-16*6);

#else

/* Unix API: callee-save regs are rbx, rbp, r12-r15 */

#define PUSH_CALLEE_SAVE_REGS \
        pushq   %rbx; CFI_ADJUST(8); CFI_OFFSET(rbx, -16); \
        pushq   %rbp; CFI_ADJUST(8); CFI_OFFSET(rbp, -24); \
        pushq   %r12; CFI_ADJUST(8); CFI_OFFSET(r12, -32); \
        pushq   %r13; CFI_ADJUST(8); CFI_OFFSET(r13, -40); \
        pushq   %r14; CFI_ADJUST(8); CFI_OFFSET(r14, -48); \
        pushq   %r15; CFI_ADJUST(8); CFI_OFFSET(r15, -56); \
        subq    $8, %rsp; CFI_ADJUST(8);

#define POP_CALLEE_SAVE_REGS \
        addq    $8, %rsp; CFI_ADJUST(-8); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#define PUSH_CALLERSAVE_INT_BASE_REGS_UNALIGNED \
        pushq   %rax; CFI_ADJUST (8); \
        pushq   %rcx; CFI_ADJUST (8); \
        pushq   %rdx; CFI_ADJUST (8); \
        pushq   %rsi; CFI_ADJUST (8); \
        pushq   %rdi; CFI_ADJUST (8); \
        pushq   %r8; CFI_ADJUST (8); \
        pushq   %r9; CFI_ADJUST (8);

#define POP_CALLERSAVE_INT_BASE_REGS_UNALIGNED \
        popq    %r9; CFI_ADJUST (-8); \
        popq    %r8; CFI_ADJUST (-8); \
        popq    %rdi; CFI_ADJUST (-8); \
        popq    %rsi; CFI_ADJUST (-8); \
        popq    %rdx; CFI_ADJUST (-8); \
        popq    %rcx; CFI_ADJUST (-8); \
        popq    %rax; CFI_ADJUST (-8);

#define PUSH_CALLERSAVE_FP_REGS \
        subq    $(16*16), %rsp; CFI_ADJUST (16*16); \
        movaps  %xmm0, 0*16(%rsp); \
        movaps  %xmm1, 1*16(%rsp); \
        movaps  %xmm2, 2*16(%rsp); \
        movaps  %xmm3, 3*16(%rsp); \
        movaps  %xmm4, 4*16(%rsp); \
        movaps  %xmm5, 5*16(%rsp); \
        movaps  %xmm6, 6*16(%rsp); \
        movaps  %xmm7, 7*16(%rsp); \
        movaps  %xmm8, 8*16(%rsp); \
        movaps  %xmm9, 9*16(%rsp); \
        movaps  %xmm10, 10*16(%rsp); \
        movaps  %xmm11, 11*16(%rsp); \
        movaps  %xmm12, 12*16(%rsp); \
        movaps  %xmm13, 13*16(%rsp); \
        movaps  %xmm14, 14*16(%rsp); \
        movaps  %xmm15, 15*16(%rsp);

#define POP_CALLERSAVE_FP_REGS \
        movaps  0*16(%rsp), %xmm0; \
        movaps  1*16(%rsp), %xmm1; \
        movaps  2*16(%rsp), %xmm2; \
        movaps  3*16(%rsp), %xmm3; \
        movaps  4*16(%rsp), %xmm4; \
        movaps  5*16(%rsp), %xmm5; \
        movaps  6*16(%rsp), %xmm6; \
        movaps  7*16(%rsp), %xmm7; \
        movaps  8*16(%rsp), %xmm8; \
        movaps  9*16(%rsp), %xmm9; \
        movaps  10*16(%rsp), %xmm10; \
        movaps  11*16(%rsp), %xmm11; \
        movaps  12*16(%rsp), %xmm12; \
        movaps  13*16(%rsp), %xmm13; \
        movaps  14*16(%rsp), %xmm14; \
        movaps  15*16(%rsp), %xmm15; \
        addq    $(16*16), %rsp; CFI_ADJUST(-16*16);

#endif


/* Special function entry that saves all C caller-save integer registers
   On entry, the (esp + 8) is 16-byte aligned -- we specialize
   here to avoid extra add/sub instructions on the esp.
   Note: we never save/restore r11 and r10
*/
#if WITH_FRAME_POINTERS
#define ENTER_C_INT_FUNCTION \
        ENTER_FUNCTION \
        PUSH_CALLERSAVE_INT_BASE_REGS_UNALIGNED \
        subq    $8, %rsp; CFI_ADJUST (8); 

#define LEAVE_C_INT_FUNCTION \
        subq    $8, %rsp; CFI_ADJUST (8); \
        POP_CALLERSAVE_INT_BASE_REGS_UNALIGNED \
        LEAVE_FUNCTION
#else
#define ENTER_C_INT_FUNCTION \
        PUSH_CALLERSAVE_INT_BASE_REGS_UNALIGNED

#define LEAVE_C_INT_FUNCTION \
        POP_CALLERSAVE_INT_BASE_REGS_UNALIGNED
#endif        

#define ENTER_C_FUNCTION \
        ENTER_C_INT_FUNCTION \
        PUSH_CALLERSAVE_FP_REGS

#define LEAVE_C_FUNCTION \
        POP_CALLERSAVE_FP_REGS \
        LEAVE_C_INT_FUNCTION        


#if defined(SYS_mingw64) || defined (SYS_cygwin)
   /* Calls from OCaml to C must reserve 32 bytes of extra stack space */
  #define PREPARE_FOR_C_CALL subq $32, %rsp; CFI_ADJUST(32)
  #define CLEANUP_AFTER_C_CALL addq $32, %rsp; CFI_ADJUST(-32)
   /* Stack probing mustn't be larger than the page size */
  #define STACK_PROBE_SIZE 4096
#else
  #define PREPARE_FOR_C_CALL
  #define CLEANUP_AFTER_C_CALL
  #define STACK_PROBE_SIZE 4096
#endif

#define MAKE_C_CALL(cfun) \
        PREPARE_FOR_C_CALL; \
        call  GCALL(cfun); \
        CLEANUP_AFTER_C_CALL;


/* Registers holding arguments of C functions. */

#if defined(SYS_mingw64) || defined(SYS_cygwin)
#define C_ARG_1 %rcx
#define C_ARG_2 %rdx
#define C_ARG_3 %r8
#define C_ARG_4 %r9
#else
#define C_ARG_1 %rdi
#define C_ARG_2 %rsi
#define C_ARG_3 %rdx
#define C_ARG_4 %rcx
#endif

        .text

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot__code_begin)
        .globl  G(caml_hot__code_begin)
G(caml_hot__code_begin):

        TEXT_SECTION(caml_hot__code_end)
        .globl  G(caml_hot__code_end)
G(caml_hot__code_end):
#endif

        TEXT_SECTION(caml_system__code_begin)
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):
        ret  /* just one instruction, so that debuggers don't display
        caml_system__code_begin instead of caml_call_gc */

/* ----------------------------------------------------------------------
   Reference counting primitives
   Written in assembly to avoid calling into C which requires saving
   the caller-save registers
-----------------------------------------------------------------------*/   

/* dup an atomic reference count
   r11: the dup argument; guaranteed pointer
   TODO: implement sticky range
*/   
FUNCTION(G(caml_rc_asm_dup_atomic))
CFI_STARTPROC
    lock    decl -4(%r11) 
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_dup_atomic))

/* drop an atomic reference count
   r11: the drop argument; guaranteed pointer
   TODO: implement sticky range
*/   
FUNCTION(G(caml_rc_asm_drop_atomic))
CFI_STARTPROC
    lock  incl -4(%r11)         /* todo: atomic drop correctly with sticky range and possibly free */
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_drop_atomic))

/* generic drop children and free the block
   r11 is the drop argument  */
FUNCTION(G(caml_rc_asm_drop_free))
CFI_STARTPROC
    ENTER_C_INT_FUNCTION
    pushq   %r10                 /* todo: make r10 a destroyed register in `proc.ml`? */
    pushq   %r10                 /* twice to keep the stack aligned */
    movq    %r11, C_ARG_1
    MAKE_C_CALL(rc_drop_free)
    popq    %r10
    popq    %r10
    LEAVE_C_INT_FUNCTION
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_drop_free))


/* generic malloc:
   r11 = size to allocate
   r11 = result, destroy r10
*/
FUNCTION(G(caml_rc_asm_alloc))
CFI_STARTPROC
    ENTER_C_FUNCTION
    movq    %r11, C_ARG_1
    MAKE_C_CALL(rc_alloc)
    movq    %rax, %r11          /* Move the result into r11 */
    LEAVE_C_FUNCTION            /* ensure r11 is not overwritten! */
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_alloc))


/* generic free: r11 is the argument, destroy r10 */
FUNCTION(G(caml_rc_asm_free))
CFI_STARTPROC
    ENTER_C_FUNCTION
    movq    %r11, C_ARG_1
    MAKE_C_CALL(rc_free)
    LEAVE_C_FUNCTION
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_free))


/* ----------------------------------------------------------------------
   Reference counting primitives
   Inlined definitions for mi_malloc's  mi_free and mi_alloc. We assume
   we can compile mimalloc without using FP registers and thus don't need
   to save those.

   The fast path for mi_malloc is emitted inline (`emit.mlp`) to specialize
   for the size.

   While the mi_free fast path is written in assembly here to avoid 
   saving too many registers.
-----------------------------------------------------------------------*/   


/* Generic mi_malloc allocation (the fast path is emitted inline) 
   r11 is the allocation size  (and r15 is the heap pointer)   
   r11 is result register, destroy r10
*/
FUNCTION(G(caml_rc_asm_mi_alloc_generic))
CFI_STARTPROC
    ENTER_C_INT_FUNCTION    
    movq    %r15, C_ARG_1             /* r14 = Caml_state, r15 = mimalloc heap pointer */
    movq    %r11, C_ARG_2
    MAKE_C_CALL(rc_mi_heap_alloc)   /* rax has the result */
    movq    %rax, %r11
    LEAVE_C_INT_FUNCTION     /* ensure r11 is not overwritten! */    
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_alloc_generic))


/* The emitted fast-path comes from inspecting mimalloc generated assembly for mi_heap_malloc_small (using -DMI_SEE_ASM=ON) */
/* rdi=heap, rsi=size
mi_heap_malloc_small:
.LFB654:
	.cfi_startproc
	endbr64
	leaq	7(%rsi), %rax
	shrq	$3, %rax
	movq	8(%rdi,%rax,8), %rax
	movq	16(%rax), %r8
	testq	%r8, %r8
	je	.L52
	movq	(%r8), %rdx
	addl	$1, 24(%rax)
	movq	%rdx, 16(%rax)
	movq	%r8, %rax
	ret
	.p2align 4,,10
	.p2align 3
.L52:
	xorl	%ecx, %ecx        // zero?
	xorl	%edx, %edx        // huge_alignment
	jmp	_mi_malloc_generic@PLT
*/


/* r11 is the free argument pointing to the OCaml block (8 bytes beyond the allocated heap block)
   It should not be NULL 
   Destroys: rax (temp), r10 (segment/block) (and r11)
   Note: try to use minimal registers (r11,r10,rax) -- we do spill the block on the stack now though...
*/
FUNCTION(G(caml_rc_asm_mi_free))
  leaq    -8(%r11), %r11             /* adjust r11 to point to the heap block itself */
  /* and fall through */
ENDFUNCTION(G(caml_rc_asm_mi_free))

/* fast entry with r11 pointing to the allocated heap block */
FUNCTION(G(caml_rc_asm_mi_free_hp))
CFI_STARTPROC
  /* no need to test for NULL */
  /* no need to do -1 since we won't have large alignments */
  pushq   %r11                       /* save block on stack (later pop'd in rax) */
  movq    %r11, %rax
  andq    $-33554432, %r11           /* r11 for the segment  (r11 &= ~(0x2000000 - 1)) = 32MiB segments */
  
  subq    %r11, %rax
  shrq    $16, %rax                   /* 64KiB pages */
  leaq	  (%rax,%rax,4), %rax         /* rax *= 5 */
  salq	  $4, %rax
  leaq	  264(%rax), %r10
  movl	  268(%r11,%rax), %eax     
  subq	  %rax, %r10     
  addq	  %r11, %r10                  /* r10 = page */
	
  /* on entry: block on the stack, r11 is the segment, r10 is the page */
  /* note: we can potentially skip the thread local check as we only call mi_free if the block was unique and not thread shared */
  movq    %fs:0, %rax                 /* rax = thread id */
  cmpq    256(%r11), %rax             /* thread_id == segment->thread_id ? */
  popq    %rax                        /* load rax to point to the block */  
  jne     GCALL(caml_rc_asm_mi_free_generic_nonlocal) /* tail call */
	cmpb	  $0, 14(%r10)                /* page is not full or aligned? */
	jne	    GCALL(caml_rc_asm_mi_free_generic_local)    /* tail call */
  
  movq	  32(%r10), %r11              /* overwrite r11, the segment, as we don't need it anymore */              
	movq	  %r11, (%rax)                /* block->next = page->local_free */
	subl	  $1, 24(%r10)                /* page->used-- */
	movq	  %rax, 32(%r10)              /* page->local_free = block */
	jz	    GCALL(caml_rc_asm_mi_page_retire)  /* is page empty? (then tail call) */
  ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_free_hp))


/* mimalloc generic free path (non thread local)
   r11 = segment, r10 = page, rax = block   
*/
FUNCTION(G(caml_rc_asm_mi_free_generic_nonlocal))
CFI_STARTPROC
    ENTER_C_INT_FUNCTION    /* todo: don't need to save rax ? */
    movq    %r11, C_ARG_1   
    movq    %r10, C_ARG_2   
    xorq    C_ARG_3, C_ARG_3
    movq    %rax, C_ARG_4
    MAKE_C_CALL(_mi_free_generic)
    LEAVE_C_INT_FUNCTION
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_free_generic_nonlocal))

/* mimalloc generic free path (thread local)
   r11 = segment, r10 = page, rax = block   
*/
FUNCTION(G(caml_rc_asm_mi_free_generic_local))
CFI_STARTPROC
    ENTER_C_INT_FUNCTION    /* todo: don't need to save rax ? */
    movq    %r11, C_ARG_1   
    movq    %r10, C_ARG_2   
    movq    $1, C_ARG_3
    movq    %rax, C_ARG_4
    MAKE_C_CALL(_mi_free_generic)
    LEAVE_C_INT_FUNCTION
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_free_generic_local))

/* r10 = page
   destroys r11
*/
FUNCTION(G(caml_rc_asm_mi_page_retire))
CFI_STARTPROC
    ENTER_C_INT_FUNCTION    /* todo: don't need to save rax ? */    
    movq    %r10, C_ARG_1
    MAKE_C_CALL(_mi_page_retire)
    LEAVE_C_INT_FUNCTION
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_page_retire))


/* The code above comes from inspecting mimalloc generated assembly for mi_free (using -DMI_SEE_ASM=ON) */
/*
mi_free:
.LFB674:
	.cfi_startproc
	endbr64
	movq	%rdi, %rcx
	testq	%rdi, %rdi
	je	.L183
	leaq	-1(%rdi), %rdi
	movq	%rcx, %rax
	andq	$-33554432, %rdi
	subq	%rdi, %rax
	shrq	$16, %rax
	leaq	(%rax,%rax,4), %rax
	salq	$4, %rax
	leaq	264(%rax), %rsi
#APP
# 867 "/home/daan/home/dev/mimalloc/include/mimalloc-internal.h" 1
	movq %fs:0, %r8
# 0 "" 2
#NO_APP
	movq	256(%rdi), %rdx
	movl	268(%rdi,%rax), %eax
	subq	%rax, %rsi
	addq	%rdi, %rsi
	cmpq	%rdx, %r8
	jne	.L186
	cmpb	$0, 14(%rsi)
	jne	.L187
	movq	32(%rsi), %rax
	movq	%rax, (%rcx)
	subl	$1, 24(%rsi)
	movq	%rcx, 32(%rsi)
	je	.L189
.L183:
	ret
	.p2align 4,,10
	.p2align 3
.L189:
	movq	%rsi, %rdi
	jmp	_mi_page_retire@PLT
	.p2align 4,,10
	.p2align 3
.L186:
	xorl	%edx, %edx
	jmp	_mi_free_generic
	.p2align 4,,10
	.p2align 3
.L187:
	movl	$1, %edx
	jmp	_mi_free_generic
*/  


/* drop N children and free the block 
   r11 = block
*/   
FUNCTION(G(caml_rc_asm_mi_dropN_free))
CFI_STARTPROC
    pushq   %r10
    pushq   %rax

LBL(caml_rc_asm_mi_dropN_free_recurse):    
    movl    -8(%r11), %eax                 /* load lower 32 bits of the header: 21-bit size ++ 2-bit color ++ 8-bit tag */
    cmpb    $247, %al                      /* compare unsigned for special tags */
    shrq    $10, %rax                      /* rax is the #fields */
    jae     LBL(caml_rc_asm_mi_dropN_free_loop_end_manyptr)  /* go to generic drop routine for special tag */

LBL(caml_rc_asm_mi_dropN_free_loop_noptr):
    testq   %rax, %rax
    movq    (%r11,%rax,8), %r10
    leaq    -1(%rax), %rax
    jz      LBL(caml_rc_asm_mi_dropN_free_loop_end_noptr)

    testq   $1, %r10
    jnz     LBL(caml_rc_asm_mi_dropN_free_loop_noptr)
    /* fall through, r10 is the pointer child */

LBL(caml_rc_asm_mi_dropN_free_loop_oneptr):    
    testq   %rax, %rax
    leaq    -1(%rax), %rax
    jz      LBL(caml_rc_asm_mi_dropN_free_loop_end_oneptr)

    testq   $1, 8(%r11,%rax,8)
    jnz     LBL(caml_rc_asm_mi_dropN_free_loop_oneptr)

LBL(caml_rc_asm_mi_dropN_free_loop_end_manyptr):
    /* more than one child is a pointer, use C generic drop */
    popq    %rax
    popq    %r10
    jmp     GCALL(caml_rc_asm_drop_free)

LBL(caml_rc_asm_mi_dropN_free_loop_end_noptr):    
    /* no child is a pointer, just free it */
    call    GCALL(caml_rc_asm_mi_free)
    popq    %rax
    popq    %r10
    ret

LBL(caml_rc_asm_mi_dropN_free_loop_end_oneptr):    
    /* one pointer child in r10, free the block and tail recurse */
    pushq   %r10
    subq    $8, %rsp
    call    GCALL(caml_rc_asm_mi_free)
    addq    $8, %rsp
    popq    %r11
    jmp     LBL(caml_rc_asm_mi_dropN_free_recurse)

CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_dropN_free))


/* ----------------------------------------------------------------------
   Old GC allocation code
-----------------------------------------------------------------------*/   

/* Allocation */

FUNCTION(G(caml_call_gc))
        CFI_STARTPROC
LBL(caml_call_gc):
    /* Record lowest stack address and return address. */
        movq    (%rsp), %r11
        movq    %r11, Caml_state(last_return_address)
        leaq    8(%rsp), %r11
        movq    %r11, Caml_state(bottom_of_stack)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE);
        movq    %r11, 0(%rsp)
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE);
    /* Build array of registers, save it into Caml_state->gc_regs */
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION          ;
#else
        pushq   %rbp; CFI_ADJUST(8);
#endif
        pushq   %r11; CFI_ADJUST (8);
        pushq   %r10; CFI_ADJUST (8);
        pushq   %r13; CFI_ADJUST (8);
        pushq   %r12; CFI_ADJUST (8);
        pushq   %r9; CFI_ADJUST (8);
        pushq   %r8; CFI_ADJUST (8);
        pushq   %rcx; CFI_ADJUST (8);
        pushq   %rdx; CFI_ADJUST (8);
        pushq   %rsi; CFI_ADJUST (8);
        pushq   %rdi; CFI_ADJUST (8);
        pushq   %rbx; CFI_ADJUST (8);
        pushq   %rax; CFI_ADJUST (8);
        movq    %rsp, Caml_state(gc_regs)
    /* Save young_ptr */
        /* movq    %r15, Caml_state(young_ptr) */        
    /* Save floating-point registers */
        subq    $(16*8), %rsp; CFI_ADJUST (16*8);
        movsd   %xmm0, 0*8(%rsp)
        movsd   %xmm1, 1*8(%rsp)
        movsd   %xmm2, 2*8(%rsp)
        movsd   %xmm3, 3*8(%rsp)
        movsd   %xmm4, 4*8(%rsp)
        movsd   %xmm5, 5*8(%rsp)
        movsd   %xmm6, 6*8(%rsp)
        movsd   %xmm7, 7*8(%rsp)
        movsd   %xmm8, 8*8(%rsp)
        movsd   %xmm9, 9*8(%rsp)
        movsd   %xmm10, 10*8(%rsp)
        movsd   %xmm11, 11*8(%rsp)
        movsd   %xmm12, 12*8(%rsp)
        movsd   %xmm13, 13*8(%rsp)
        movsd   %xmm14, 14*8(%rsp)
        movsd   %xmm15, 15*8(%rsp)
    /* Call the garbage collector */
        PREPARE_FOR_C_CALL
        call    GCALL(caml_garbage_collection)
        CLEANUP_AFTER_C_CALL
    /* Restore young_ptr */
        /* movq    Caml_state(young_ptr), %r15 */
    /* Restore all regs used by the code generator */
        movsd   0*8(%rsp), %xmm0
        movsd   1*8(%rsp), %xmm1
        movsd   2*8(%rsp), %xmm2
        movsd   3*8(%rsp), %xmm3
        movsd   4*8(%rsp), %xmm4
        movsd   5*8(%rsp), %xmm5
        movsd   6*8(%rsp), %xmm6
        movsd   7*8(%rsp), %xmm7
        movsd   8*8(%rsp), %xmm8
        movsd   9*8(%rsp), %xmm9
        movsd   10*8(%rsp), %xmm10
        movsd   11*8(%rsp), %xmm11
        movsd   12*8(%rsp), %xmm12
        movsd   13*8(%rsp), %xmm13
        movsd   14*8(%rsp), %xmm14
        movsd   15*8(%rsp), %xmm15
        addq    $(16*8), %rsp; CFI_ADJUST(-16*8)
        popq    %rax; CFI_ADJUST(-8)
        popq    %rbx; CFI_ADJUST(-8)
        popq    %rdi; CFI_ADJUST(-8)
        popq    %rsi; CFI_ADJUST(-8)
        popq    %rdx; CFI_ADJUST(-8)
        popq    %rcx; CFI_ADJUST(-8)
        popq    %r8; CFI_ADJUST(-8)
        popq    %r9; CFI_ADJUST(-8)
        popq    %r12; CFI_ADJUST(-8)
        popq    %r13; CFI_ADJUST(-8)
        popq    %r10; CFI_ADJUST(-8)
        popq    %r11; CFI_ADJUST(-8)
#ifdef WITH_FRAME_POINTERS
        LEAVE_FUNCTION
#else
        popq    %rbp; CFI_ADJUST(-8);
#endif
    /* Return to caller */
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_call_gc))

FUNCTION(G(caml_alloc1))
CFI_STARTPROC
        subq    $16, %r15
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc1))

FUNCTION(G(caml_alloc2))
CFI_STARTPROC
        subq    $24, %r15
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc2))

FUNCTION(G(caml_alloc3))
CFI_STARTPROC
        subq    $32, %r15
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_alloc3))

FUNCTION(G(caml_allocN))
CFI_STARTPROC
        cmpq    Caml_state(young_limit), %r15
        jb      LBL(caml_call_gc)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_allocN))


/* ----------------------------------------------------------------------
   Other
-----------------------------------------------------------------------*/   

/* Call a C function from OCaml */

FUNCTION(G(caml_c_call))
CFI_STARTPROC
LBL(caml_c_call):
    /* Record lowest stack address and return address */
        popq    Caml_state(last_return_address); CFI_ADJUST(-8)
        movq    %rsp, Caml_state(bottom_of_stack)
    /* equivalent to pushing last return address */
        subq    $8, %rsp; CFI_ADJUST(8)
    /* Touch the stack to trigger a recoverable segfault
       if insufficient space remains */
        subq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(STACK_PROBE_SIZE);
        movq    %rax, 0(%rsp)
        addq    $(STACK_PROBE_SIZE), %rsp; CFI_ADJUST(-STACK_PROBE_SIZE);
    /* Make the alloc ptr available to the C code */
       /* movq    %r15, Caml_state(young_ptr) */
    /* Call the function (address in %rax) */
    /* No need to PREPARE_FOR_C_CALL since the caller already
       reserved the stack space if needed (cf. amd64/proc.ml) */
        jmp    *%rax
CFI_ENDPROC
ENDFUNCTION(G(caml_c_call))

/* Start the OCaml program */

FUNCTION(G(caml_start_program))
       CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Load Caml_state into r14 (was passed as an argument from C) */
        movq    C_ARG_1, %r14
    /* Initial entry point is G(caml_program) */
        LEA_VAR(caml_program, %r12)
    /* Common code for caml_start_program and caml_callback* */
LBL(caml_start_program):
    /* Build a callback link */
        subq    $8, %rsp; CFI_ADJUST (8)        /* stack 16-aligned */
        pushq   Caml_state(gc_regs); CFI_ADJUST(8)
        pushq   Caml_state(last_return_address); CFI_ADJUST(8)
        pushq   Caml_state(bottom_of_stack); CFI_ADJUST(8)
    /* setup mi_heap */
        call    G(mi_heap_get_default)
        movq    %rax, Caml_state(mi_heap)
        movq    %rax, %r15
    /* Setup alloc ptr */
        /* movq    Caml_state(young_ptr), %r15 */
    /* Build an exception handler */
        lea     LBL(108)(%rip), %r13
        pushq   %r13; CFI_ADJUST(8)
        pushq   Caml_state(exception_pointer); CFI_ADJUST(8)
        movq    %rsp, Caml_state(exception_pointer)
    /* Call the OCaml code */
        call    *%r12
LBL(107):
    /* Pop the exception handler */
        popq    Caml_state(exception_pointer); CFI_ADJUST(-8)
        popq    %r12; CFI_ADJUST(-8)   /* dummy register */
LBL(109):
    /* Update alloc ptr */
        /* movq    %r15, Caml_state(young_ptr) */
    /* Pop the callback link, restoring the global variables */
        popq    Caml_state(bottom_of_stack); CFI_ADJUST(-8)
        popq    Caml_state(last_return_address); CFI_ADJUST(-8)
        popq    Caml_state(gc_regs); CFI_ADJUST(-8)
        addq    $8, %rsp; CFI_ADJUST (-8);
    /* Restore callee-save registers. */
        POP_CALLEE_SAVE_REGS
    /* Return to caller. */
        ret
LBL(108):
    /* Exception handler*/
    /* Mark the bucket as an exception result and return it */
        orq     $2, %rax
        jmp     LBL(109)
CFI_ENDPROC
ENDFUNCTION(G(caml_start_program))

/* Raise an exception from OCaml */

FUNCTION(G(caml_raise_exn))
CFI_STARTPROC
        testq   $1, Caml_state(backtrace_active)
        jne     LBL(110)
        movq    Caml_state(exception_pointer), %rsp
        popq    Caml_state(exception_pointer); CFI_ADJUST(-8)
        ret
LBL(110):
        movq    %rax, %r12            /* Save exception bucket */
        movq    %rax, C_ARG_1         /* arg 1: exception bucket */
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION
        movq    8(%rsp), C_ARG_2      /* arg 2: pc of raise */
        leaq    16(%rsp), C_ARG_3     /* arg 3: sp at raise */
#else
        popq    C_ARG_2               /* arg 2: pc of raise */
        movq    %rsp, C_ARG_3         /* arg 3: sp at raise */
#endif
        /* arg 4: sp of handler */
        movq    Caml_state(exception_pointer), C_ARG_4
        /* PR#5700: thanks to popq above, stack is now 16-aligned */
        /* Thanks to ENTER_FUNCTION, stack is now 16-aligned */
        PREPARE_FOR_C_CALL            /* no need to cleanup after */
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
        movq    Caml_state(exception_pointer), %rsp
        popq    Caml_state(exception_pointer); CFI_ADJUST(-8)
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exn))

/* Raise an exception from C */

FUNCTION(G(caml_raise_exception))
CFI_STARTPROC
        movq    C_ARG_1, %r14   /* Caml_state */
        testq   $1, Caml_state(backtrace_active)
        jne     LBL(112)
        movq    C_ARG_2, %rax
        movq    Caml_state(exception_pointer), %rsp  /* Cut stack */
        /* Recover previous exception handler */
        popq    Caml_state(exception_pointer); CFI_ADJUST(-8)
        /* movq    Caml_state(young_ptr), %r15 */ /* Reload alloc ptr */
        ret
LBL(112):
#ifdef WITH_FRAME_POINTERS
        ENTER_FUNCTION          ;
#endif
        /* Save exception bucket. Caml_state in r14 saved across C calls. */
        movq    C_ARG_2, %r12
        /* arg 1: exception bucket */
        movq    C_ARG_2, C_ARG_1
        /* arg 2: pc of raise */
        movq    Caml_state(last_return_address), C_ARG_2
        /* arg 3: sp of raise */
        movq    Caml_state(bottom_of_stack), C_ARG_3
        /* arg 4: sp of handler */
        movq    Caml_state(exception_pointer), C_ARG_4
#ifndef WITH_FRAME_POINTERS
        subq    $8, %rsp              /* PR#5700: maintain stack alignment */
#endif
        PREPARE_FOR_C_CALL            /* no need to cleanup after */
        call    GCALL(caml_stash_backtrace)
        movq    %r12, %rax            /* Recover exception bucket */
        movq    Caml_state(exception_pointer), %rsp
     /* Recover previous exception handler */
        popq    Caml_state(exception_pointer); CFI_ADJUST(-8)
        /* movq    Caml_state(young_ptr), %r15 */ /* Reload alloc ptr */ 
        ret
CFI_ENDPROC
ENDFUNCTION(G(caml_raise_exception))

/* Raise a Stack_overflow exception on return from segv_handler()
   (in runtime/signals_nat.c).  On entry, the stack is full, so we
   cannot record a backtrace.
   No CFI information here since this function disrupts the stack
   backtrace anyway. */

FUNCTION(G(caml_stack_overflow))
        movq    C_ARG_1, %r14                 /* Caml_state */
        LEA_VAR(caml_exn_Stack_overflow, %rax)
        movq    Caml_state(exception_pointer), %rsp /* cut the stack */
     /* Recover previous exn handler */
        popq    Caml_state(exception_pointer)
        ret                                   /* jump to handler's code */
ENDFUNCTION(G(caml_stack_overflow))

/* Callback from C to OCaml */

FUNCTION(G(caml_callback_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rbx      /* closure */
        movq    0(C_ARG_3), %rax   /* argument */
        movq    0(%rbx), %r12      /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback_asm))

FUNCTION(G(caml_callback2_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    C_ARG_2, %rdi      /* closure */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        LEA_VAR(caml_apply2, %r12) /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback2_asm))

FUNCTION(G(caml_callback3_asm))
CFI_STARTPROC
    /* Save callee-save registers */
        PUSH_CALLEE_SAVE_REGS
    /* Initial loading of arguments */
        movq    C_ARG_1, %r14      /* Caml_state */
        movq    0(C_ARG_3), %rax   /* first argument */
        movq    8(C_ARG_3), %rbx   /* second argument */
        movq    C_ARG_2, %rsi      /* closure */
        movq    16(C_ARG_3), %rdi  /* third argument */
        LEA_VAR(caml_apply3, %r12) /* code pointer */
        jmp     LBL(caml_start_program)
CFI_ENDPROC
ENDFUNCTION(G(caml_callback3_asm))

FUNCTION(G(caml_ml_array_bound_error))
CFI_STARTPROC
        LEA_VAR(caml_array_bound_error, %rax)
        jmp     LBL(caml_c_call)
CFI_ENDPROC
ENDFUNCTION(G(caml_ml_array_bound_error))

        TEXT_SECTION(caml_system__code_end)
        .globl  G(caml_system__code_end)
G(caml_system__code_end):

        .data
        .globl  G(caml_system__frametable)
        .align  EIGHT_ALIGN
G(caml_system__frametable):
        .quad   1           /* one descriptor */
        .quad   LBL(107)    /* return address into callback */
        .value  -1          /* negative frame size => use callback link */
        .value  0           /* no roots here */
        .align  EIGHT_ALIGN
        .quad   16
        .quad   0
        .string "amd64.S"

#if defined(SYS_macosx)
        .literal16
#elif defined(SYS_mingw64) || defined(SYS_cygwin)
        .section .rdata,"dr"
#else
        .section    .rodata.cst16,"aM",@progbits,16
#endif
        .globl  G(caml_negf_mask)
        .align  SIXTEEN_ALIGN
G(caml_negf_mask):
        .quad   0x8000000000000000, 0
        .globl  G(caml_absf_mask)
        .align  SIXTEEN_ALIGN
G(caml_absf_mask):
        .quad   0x7FFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF

#if defined(SYS_linux)
    /* Mark stack as non-executable, PR#4564 */
        .section .note.GNU-stack,"",%progbits
#endif
