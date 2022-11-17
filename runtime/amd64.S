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
   Keep the stack 16-aligned. */

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
        movupd  %xmm15, 9*16(%rsp)

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

#define PUSH_CALLER_SAVE_INT_BASE_REGS \
        pushq   %rax; CFI_ADJUST (8);  \
        pushq   %rcx; CFI_ADJUST (8);  \
        pushq   %rdx; CFI_ADJUST (8);  \
        pushq   %r8; CFI_ADJUST (8);   \
        pushq   %r9; CFI_ADJUST (8);

#define POP_CALLER_SAVE_INT_BASE_REGS \
        popq    %r9; CFI_ADJUST (-8); \
        popq    %r8; CFI_ADJUST (-8); \
        popq    %rdx; CFI_ADJUST (-8); \
        popq    %rcx; CFI_ADJUST (-8); \
        popq    %rax; CFI_ADJUST (-8);

#define PUSH_CALLER_SAVE_FP_REGS \
        subq    $(16*6), %rsp; CFI_ADJUST (16*6); \
        movsd   %xmm0, 0*8(%rsp); \
        movsd   %xmm1, 1*8(%rsp); \
        movsd   %xmm2, 2*8(%rsp); \
        movsd   %xmm3, 3*8(%rsp); \
        movsd   %xmm4, 4*8(%rsp); \
        movsd   %xmm5, 5*8(%rsp);


#define POP_CALLER_SAVE_FP_REGS \
        movsd   0*8(%rsp), %xmm0; \
        movsd   1*8(%rsp), %xmm1; \
        movsd   2*8(%rsp), %xmm2; \
        movsd   3*8(%rsp), %xmm3; \
        movsd   4*8(%rsp), %xmm4; \
        movsd   5*8(%rsp), %xmm5; \
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
        subq    $8, %rsp; CFI_ADJUST(8)

#define POP_CALLEE_SAVE_REGS \
        addq    $8, %rsp; CFI_ADJUST(-8); \
        popq    %r15; CFI_ADJUST(-8); CFI_SAME_VALUE(r15); \
        popq    %r14; CFI_ADJUST(-8); CFI_SAME_VALUE(r14); \
        popq    %r13; CFI_ADJUST(-8); CFI_SAME_VALUE(r13); \
        popq    %r12; CFI_ADJUST(-8); CFI_SAME_VALUE(r12); \
        popq    %rbp; CFI_ADJUST(-8); CFI_SAME_VALUE(rbp); \
        popq    %rbx; CFI_ADJUST(-8); CFI_SAME_VALUE(rbx)

#define PUSH_CALLER_SAVE_INT_BASE_REGS \
        pushq   %rax; CFI_ADJUST (8); \
        pushq   %rcx; CFI_ADJUST (8); \
        pushq   %rdx; CFI_ADJUST (8); \
        pushq   %rsi; CFI_ADJUST (8); \
        pushq   %rdi; CFI_ADJUST (8); \
        pushq   %r8; CFI_ADJUST (8); \
        pushq   %r9; CFI_ADJUST (8);

#define POP_CALLER_SAVE_INT_BASE_REGS \
        popq    %r9; CFI_ADJUST (-8); \
        popq    %r8; CFI_ADJUST (-8); \
        popq    %rdi; CFI_ADJUST (-8); \
        popq    %rsi; CFI_ADJUST (-8); \
        popq    %rdx; CFI_ADJUST (-8); \
        popq    %rcx; CFI_ADJUST (-8); \
        popq    %rax; CFI_ADJUST (-8);

#define PUSH_CALLER_SAVE_FP_REGS \
        subq    $(16*8), %rsp; CFI_ADJUST (16*8); \
        movsd   %xmm0, 0*8(%rsp); \
        movsd   %xmm1, 1*8(%rsp); \
        movsd   %xmm2, 2*8(%rsp); \
        movsd   %xmm3, 3*8(%rsp); \
        movsd   %xmm4, 4*8(%rsp); \
        movsd   %xmm5, 5*8(%rsp); \
        movsd   %xmm6, 6*8(%rsp); \
        movsd   %xmm7, 7*8(%rsp); \
        movsd   %xmm8, 8*8(%rsp); \
        movsd   %xmm9, 9*8(%rsp); \
        movsd   %xmm10, 10*8(%rsp); \
        movsd   %xmm11, 11*8(%rsp); \
        movsd   %xmm12, 12*8(%rsp); \
        movsd   %xmm13, 13*8(%rsp); \
        movsd   %xmm14, 14*8(%rsp); \
        movsd   %xmm15, 15*8(%rsp);

#define POP_CALLER_SAVE_FP_REGS \
        movsd   0*8(%rsp), %xmm0; \
        movsd   1*8(%rsp), %xmm1; \
        movsd   2*8(%rsp), %xmm2; \
        movsd   3*8(%rsp), %xmm3; \
        movsd   4*8(%rsp), %xmm4; \
        movsd   5*8(%rsp), %xmm5; \
        movsd   6*8(%rsp), %xmm6; \
        movsd   7*8(%rsp), %xmm7; \
        movsd   8*8(%rsp), %xmm8; \
        movsd   9*8(%rsp), %xmm9; \
        movsd   10*8(%rsp), %xmm10; \
        movsd   11*8(%rsp), %xmm11; \
        movsd   12*8(%rsp), %xmm12; \
        movsd   13*8(%rsp), %xmm13; \
        movsd   14*8(%rsp), %xmm14; \
        movsd   15*8(%rsp), %xmm15; \
        addq    $(16*8), %rsp; CFI_ADJUST(-16*8);

#endif

#define PUSH_CALLER_SAVE_INT_REGS \
        PUSH_CALLER_SAVE_INT_BASE_REGS \
        pushq   %r10; CFI_ADJUST (8); \
        pushq   %r11; CFI_ADJUST (8);

#define POP_CALLER_SAVE_INT_REGS \
        popq    %r11; CFI_ADJUST (-8); \
        popq    %r10; CFI_ADJUST (-8); \
        POP_CALLER_SAVE_INT_BASE_REGS

#define PUSH_CALLER_SAVE_REGS \
        PUSH_CALLER_SAVE_INT_REGS \
        PUSH_CALLER_SAVE_FP_REGS

#define POP_CALLER_SAVE_REGS \
        POP_CALLER_SAVE_FP_REGS \
        POP_CALLER_SAVE_INT_REGS
        


#if defined(SYS_mingw64) || defined (SYS_cygwin)
   /* Calls from OCaml to C must reserve 32 bytes of extra stack space */
#  define PREPARE_FOR_C_CALL subq $32, %rsp; CFI_ADJUST(32)
#  define CLEANUP_AFTER_C_CALL addq $32, %rsp; CFI_ADJUST(-32)
   /* Stack probing mustn't be larger than the page size */
#  define STACK_PROBE_SIZE 4096
#else
#  define PREPARE_FOR_C_CALL
#  define CLEANUP_AFTER_C_CALL
#  define STACK_PROBE_SIZE 4096
#endif

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
    PUSH_CALLER_SAVE_INT_REGS   /* todo: save all registers (not needed for mimalloc?) */
    movq    %r11, C_ARG_1
    PREPARE_FOR_C_CALL
    call    GCALL(rc_drop_free)
    CLEANUP_AFTER_C_CALL
    POP_CALLER_SAVE_INT_REGS
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_drop_free))


/* generic malloc:
   r11 = size to allocate
   r11 = result, destroy r10
*/
FUNCTION(G(caml_rc_asm_alloc))
CFI_STARTPROC
    PUSH_CALLER_SAVE_INT_BASE_REGS
    PUSH_CALLER_SAVE_FP_REGS
    movq    %r11, C_ARG_1
    PREPARE_FOR_C_CALL
    call    GCALL(rc_alloc)
    CLEANUP_AFTER_C_CALL    
    movq    %rax, %r11                /* Move the result into r11 */
    POP_CALLER_SAVE_FP_REGS
    POP_CALLER_SAVE_INT_BASE_REGS     /* ensure r11 is not overwritten! */
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_alloc))


/* generic free: r11 is the argument, destroy r10 */
FUNCTION(G(caml_rc_asm_free))
CFI_STARTPROC
    PUSH_CALLER_SAVE_INT_BASE_REGS
    PUSH_CALLER_SAVE_FP_REGS
    movq    %r11, C_ARG_1
    PREPARE_FOR_C_CALL
    call    GCALL(rc_free)
    CLEANUP_AFTER_C_CALL
    POP_CALLER_SAVE_FP_REGS
    POP_CALLER_SAVE_INT_BASE_REGS
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
    PUSH_CALLER_SAVE_INT_BASE_REGS    
    movq    %r15, C_ARG_1             /* r14 = Caml_state, r15 = mimalloc heap pointer */
    movq    %r11, C_ARG_2
    PREPARE_FOR_C_CALL
    call    GCALL(rc_mi_heap_alloc)   /* rax has the result */
    CLEANUP_AFTER_C_CALL
    movq    %rax, %r11
    POP_CALLER_SAVE_INT_BASE_REGS     /* ensure r11 is not overwritten! */
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_alloc_generic))


/* The emitted fast-path comes from inspecting mimalloc generated assembly for mi_heap_malloc_small (using -DMI_SEE_ASM=ON) */
/* 
mi_heap_malloc_small:
	endbr64
	leaq	7(%rsi), %rax
	shrq	$3, %rax
	movq	8(%rdi,%rax,8), %rax
	movq	16(%rax), %r8
	testq	%r8, %r8
	je	.L73  
	movq	(%r8), %rdx
	addl	$1, 24(%rax)
	movq	%rdx, 16(%rax)
	movq	%r8, %rax
	ret
*/


/* mimalloc generic free path 
   r11 = segment, r10 = block, rax = is_local   
*/
FUNCTION(G(caml_rc_asm_mi_free_generic))
CFI_STARTPROC
    PUSH_CALLER_SAVE_INT_BASE_REGS    /* todo: don't need to save rax ? */
    mov     %r11, C_ARG_1   
    mov     %rax, C_ARG_2   
    mov     %r10, C_ARG_3
    PREPARE_FOR_C_CALL
    call    GCALL(mi_free_generic)
    CLEANUP_AFTER_C_CALL
    POP_CALLER_SAVE_INT_BASE_REGS
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_free_generic))

/* r10 = page
   destroys r11
*/
FUNCTION(G(caml_rc_asm_mi_page_retire))
CFI_STARTPROC
    PUSH_CALLER_SAVE_INT_BASE_REGS    /* todo: don't need to save rax ? */    
    movq    %r10, C_ARG_1
    PREPARE_FOR_C_CALL
    call    GCALL(_mi_page_retire)
    CLEANUP_AFTER_C_CALL
    POP_CALLER_SAVE_INT_BASE_REGS
    ret
CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_page_retire))


/* r11 is the free argument pointing to the heap block (!) (i.e. the OCaml header); should not be NULL 
   destroys: rax (temp), r10 (segment/block) (and r11)
   note: try to use minimal registers (r11,r10,rax) -- we do spill the block on the stack now though...
*/
FUNCTION(G(caml_rc_asm_mi_free))
CFI_STARTPROC
  /* no need to test for NULL */
  /* no need to do -1 since we won't have large alignments */
  pushq   %r11                       /* save block on stack */
  movq    %r11, %rax
  andq    $-67108864, %r11           /* r11 for the segment now (r11 &= ~(0x4000000 - 1)) */
  
  subq    %r11, %rax
  shrq    $16, %rax
  leaq	  (%rax,%rax,4), %rax
  salq	  $4, %rax
  leaq	  392(%rax), %r10
  movl	  396(%r11,%rax), %eax     
  subq	  %rax, %r10     
  addq	  %r11, %r10                  /* r10 = page */
	
  /* on entry: block on the stack, r11 is the segment, r10 is the page */
  /* note: we can potentially skip the thread local check as we only call mi_free if the block was unique and not thread shared */
  movq    %fs:0, %rax                 /* rax = thread id */
  cmpq    376(%r11), %rax             /* thread_id == segment->thread_id ? */
  jne     LBL(caml_rc_asm_free_slow)
	cmpb	  $0, 14(%r10)                /* page is not full or aligned? */
	jne	    LBL(caml_rc_asm_free_slow)
  
  popq    %r11                        /* r11 is the block again (we don't need the segment anymore) */
	movq	  32(%r10), %rax              
	movq	  %rax, (%r11)                /* block->next = page->local_free */
	subl	  $1, 24(%r10)                /* page->used-- */
	movq	  %r11, 32(%r10)              /* page->local_free = block */
	jz	    LBL(caml_rc_asm_free_retire)   /* is page empty? */
  ret

LBL(caml_rc_asm_free_retire):
  /* on entry: nothing on the stack, r10 is the page */
  jmp     GCALL(caml_rc_asm_mi_page_retire)     

LBL(caml_rc_asm_free_slow):
  /* on entry: block on the stack, r11 is the segment */
  xorl	  %eax, %eax                          
	movq    %fs:0, %r10                         /* note: page is no longer needed */
	cmpq	  376(%r11), %r10
	sete	  %al                                 /* rax= (tid==thread_id) */
  pop     %r10                                /* pop block in r10 */
	jmp	    GCALL(caml_rc_asm_mi_free_generic)  /* segment in r11, tid==thread_id in rax, block in r10 */

CFI_ENDPROC
ENDFUNCTION(G(caml_rc_asm_mi_free))

/* The code above comes from inspecting mimalloc generated assembly for mi_free (using -DMI_SEE_ASM=ON) */
/*
mi_free:
.LFB674:
	.cfi_startproc
	endbr64
	movq	%rdi, %rdx
	testq	%rdi, %rdi
	je	.L141
	leaq	-1(%rdi), %r8
	andq	$-67108864, %r8
	je	.L141
	movq	%rdi, %rax
	subq	%r8, %rax
	shrq	$16, %rax
	leaq	(%rax,%rax,4), %rax
	salq	$4, %rax
	leaq	392(%rax), %rdi
	movl	396(%r8,%rax), %eax
#APP
# 864 "/home/daan/home/dev/ocaml/mimalloc/include/mimalloc-internal.h" 1
	movq %fs:0, %rcx
# 0 "" 2
#NO_APP
	subq	%rax, %rdi
	movq	376(%r8), %rax
	addq	%r8, %rdi
	cmpq	%rcx, %rax
	jne	.L145
	cmpb	$0, 14(%rdi)
	jne	.L145
	movq	32(%rdi), %rax
	movq	%rax, (%rdx)
	subl	$1, 24(%rdi)
	movq	%rdx, 32(%rdi)
	je	.L149
.L141:
	ret
	.p2align 4,,10
	.p2align 3
.L149:
	jmp	_mi_page_retire@PLT
	.p2align 4,,10
	.p2align 3
.L145:
	xorl	%esi, %esi
	movq	%r8, %rdi
	movq	376(%r8), %rax
	cmpq	%rcx, %rax
	sete	%sil
	jmp	mi_free_generic
	.cfi_endproc
*/  


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
