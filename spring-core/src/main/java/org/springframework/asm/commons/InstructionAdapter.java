/***
 * ASM: a very small and fast Java bytecode manipulation framework
 * Copyright (c) 2000-2011 INRIA, France Telecom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.springframework.asm.commons;

import org.springframework.asm.Handle;
import org.springframework.asm.Label;
import org.springframework.asm.MethodVisitor;
import org.springframework.asm.Type;

/**
 * A {@link MethodVisitor} providing a more detailed API to generate and
 * transform instructions.
 *
 * @author Eric Bruneton
 */
public class InstructionAdapter extends MethodVisitor {

    public final static Type OBJECT_TYPE = Type.getType("Ljava/lang/Object;");

    /**
     * Creates a new {@link InstructionAdapter}. <i>Subclasses must not use this
     * constructor</i>. Instead, they must use the
     * {@link #InstructionAdapter(int, MethodVisitor)} version.
     *
     * @param mv the method visitor to which this adapter delegates calls.
     */
    public InstructionAdapter(final MethodVisitor mv) {
        this(org.springframework.asm.Opcodes.ASM4, mv);
    }

    /**
     * Creates a new {@link InstructionAdapter}.
     *
     * @param api the ASM API version implemented by this visitor. Must be one
     *        of {@link org.springframework.asm.Opcodes#ASM4}.
     * @param mv the method visitor to which this adapter delegates calls.
     */
    protected InstructionAdapter(final int api, final MethodVisitor mv) {
        super(api, mv);
    }

    @Override
    public void visitInsn(final int opcode) {
        switch (opcode) {
            case org.springframework.asm.Opcodes.NOP:
                nop();
                break;
            case org.springframework.asm.Opcodes.ACONST_NULL:
                aconst(null);
                break;
            case org.springframework.asm.Opcodes.ICONST_M1:
            case org.springframework.asm.Opcodes.ICONST_0:
            case org.springframework.asm.Opcodes.ICONST_1:
            case org.springframework.asm.Opcodes.ICONST_2:
            case org.springframework.asm.Opcodes.ICONST_3:
            case org.springframework.asm.Opcodes.ICONST_4:
            case org.springframework.asm.Opcodes.ICONST_5:
                iconst(opcode - org.springframework.asm.Opcodes.ICONST_0);
                break;
            case org.springframework.asm.Opcodes.LCONST_0:
            case org.springframework.asm.Opcodes.LCONST_1:
                lconst(opcode - org.springframework.asm.Opcodes.LCONST_0);
                break;
            case org.springframework.asm.Opcodes.FCONST_0:
            case org.springframework.asm.Opcodes.FCONST_1:
            case org.springframework.asm.Opcodes.FCONST_2:
                fconst(opcode - org.springframework.asm.Opcodes.FCONST_0);
                break;
            case org.springframework.asm.Opcodes.DCONST_0:
            case org.springframework.asm.Opcodes.DCONST_1:
                dconst(opcode - org.springframework.asm.Opcodes.DCONST_0);
                break;
            case org.springframework.asm.Opcodes.IALOAD:
                aload(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LALOAD:
                aload(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FALOAD:
                aload(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DALOAD:
                aload(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.AALOAD:
                aload(OBJECT_TYPE);
                break;
            case org.springframework.asm.Opcodes.BALOAD:
                aload(Type.BYTE_TYPE);
                break;
            case org.springframework.asm.Opcodes.CALOAD:
                aload(Type.CHAR_TYPE);
                break;
            case org.springframework.asm.Opcodes.SALOAD:
                aload(Type.SHORT_TYPE);
                break;
            case org.springframework.asm.Opcodes.IASTORE:
                astore(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LASTORE:
                astore(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FASTORE:
                astore(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DASTORE:
                astore(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.AASTORE:
                astore(OBJECT_TYPE);
                break;
            case org.springframework.asm.Opcodes.BASTORE:
                astore(Type.BYTE_TYPE);
                break;
            case org.springframework.asm.Opcodes.CASTORE:
                astore(Type.CHAR_TYPE);
                break;
            case org.springframework.asm.Opcodes.SASTORE:
                astore(Type.SHORT_TYPE);
                break;
            case org.springframework.asm.Opcodes.POP:
                pop();
                break;
            case org.springframework.asm.Opcodes.POP2:
                pop2();
                break;
            case org.springframework.asm.Opcodes.DUP:
                dup();
                break;
            case org.springframework.asm.Opcodes.DUP_X1:
                dupX1();
                break;
            case org.springframework.asm.Opcodes.DUP_X2:
                dupX2();
                break;
            case org.springframework.asm.Opcodes.DUP2:
                dup2();
                break;
            case org.springframework.asm.Opcodes.DUP2_X1:
                dup2X1();
                break;
            case org.springframework.asm.Opcodes.DUP2_X2:
                dup2X2();
                break;
            case org.springframework.asm.Opcodes.SWAP:
                swap();
                break;
            case org.springframework.asm.Opcodes.IADD:
                add(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LADD:
                add(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FADD:
                add(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DADD:
                add(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.ISUB:
                sub(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LSUB:
                sub(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FSUB:
                sub(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DSUB:
                sub(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.IMUL:
                mul(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LMUL:
                mul(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FMUL:
                mul(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DMUL:
                mul(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.IDIV:
                div(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LDIV:
                div(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FDIV:
                div(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DDIV:
                div(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.IREM:
                rem(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LREM:
                rem(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FREM:
                rem(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DREM:
                rem(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.INEG:
                neg(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LNEG:
                neg(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FNEG:
                neg(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DNEG:
                neg(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.ISHL:
                shl(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LSHL:
                shl(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.ISHR:
                shr(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LSHR:
                shr(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.IUSHR:
                ushr(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LUSHR:
                ushr(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.IAND:
                and(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LAND:
                and(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.IOR:
                or(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LOR:
                or(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.IXOR:
                xor(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LXOR:
                xor(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.I2L:
                cast(Type.INT_TYPE, Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.I2F:
                cast(Type.INT_TYPE, Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.I2D:
                cast(Type.INT_TYPE, Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.L2I:
                cast(Type.LONG_TYPE, Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.L2F:
                cast(Type.LONG_TYPE, Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.L2D:
                cast(Type.LONG_TYPE, Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.F2I:
                cast(Type.FLOAT_TYPE, Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.F2L:
                cast(Type.FLOAT_TYPE, Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.F2D:
                cast(Type.FLOAT_TYPE, Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.D2I:
                cast(Type.DOUBLE_TYPE, Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.D2L:
                cast(Type.DOUBLE_TYPE, Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.D2F:
                cast(Type.DOUBLE_TYPE, Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.I2B:
                cast(Type.INT_TYPE, Type.BYTE_TYPE);
                break;
            case org.springframework.asm.Opcodes.I2C:
                cast(Type.INT_TYPE, Type.CHAR_TYPE);
                break;
            case org.springframework.asm.Opcodes.I2S:
                cast(Type.INT_TYPE, Type.SHORT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LCMP:
                lcmp();
                break;
            case org.springframework.asm.Opcodes.FCMPL:
                cmpl(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.FCMPG:
                cmpg(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DCMPL:
                cmpl(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.DCMPG:
                cmpg(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.IRETURN:
                areturn(Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LRETURN:
                areturn(Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FRETURN:
                areturn(Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DRETURN:
                areturn(Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.ARETURN:
                areturn(OBJECT_TYPE);
                break;
            case org.springframework.asm.Opcodes.RETURN:
                areturn(Type.VOID_TYPE);
                break;
            case org.springframework.asm.Opcodes.ARRAYLENGTH:
                arraylength();
                break;
            case org.springframework.asm.Opcodes.ATHROW:
                athrow();
                break;
            case org.springframework.asm.Opcodes.MONITORENTER:
                monitorenter();
                break;
            case org.springframework.asm.Opcodes.MONITOREXIT:
                monitorexit();
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitIntInsn(final int opcode, final int operand) {
        switch (opcode) {
            case org.springframework.asm.Opcodes.BIPUSH:
                iconst(operand);
                break;
            case org.springframework.asm.Opcodes.SIPUSH:
                iconst(operand);
                break;
            case org.springframework.asm.Opcodes.NEWARRAY:
                switch (operand) {
                    case org.springframework.asm.Opcodes.T_BOOLEAN:
                        newarray(Type.BOOLEAN_TYPE);
                        break;
                    case org.springframework.asm.Opcodes.T_CHAR:
                        newarray(Type.CHAR_TYPE);
                        break;
                    case org.springframework.asm.Opcodes.T_BYTE:
                        newarray(Type.BYTE_TYPE);
                        break;
                    case org.springframework.asm.Opcodes.T_SHORT:
                        newarray(Type.SHORT_TYPE);
                        break;
                    case org.springframework.asm.Opcodes.T_INT:
                        newarray(Type.INT_TYPE);
                        break;
                    case org.springframework.asm.Opcodes.T_FLOAT:
                        newarray(Type.FLOAT_TYPE);
                        break;
                    case org.springframework.asm.Opcodes.T_LONG:
                        newarray(Type.LONG_TYPE);
                        break;
                    case org.springframework.asm.Opcodes.T_DOUBLE:
                        newarray(Type.DOUBLE_TYPE);
                        break;
                    default:
                        throw new IllegalArgumentException();
                }
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitVarInsn(final int opcode, final int var) {
        switch (opcode) {
            case org.springframework.asm.Opcodes.ILOAD:
                load(var, Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LLOAD:
                load(var, Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FLOAD:
                load(var, Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DLOAD:
                load(var, Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.ALOAD:
                load(var, OBJECT_TYPE);
                break;
            case org.springframework.asm.Opcodes.ISTORE:
                store(var, Type.INT_TYPE);
                break;
            case org.springframework.asm.Opcodes.LSTORE:
                store(var, Type.LONG_TYPE);
                break;
            case org.springframework.asm.Opcodes.FSTORE:
                store(var, Type.FLOAT_TYPE);
                break;
            case org.springframework.asm.Opcodes.DSTORE:
                store(var, Type.DOUBLE_TYPE);
                break;
            case org.springframework.asm.Opcodes.ASTORE:
                store(var, OBJECT_TYPE);
                break;
            case org.springframework.asm.Opcodes.RET:
                ret(var);
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitTypeInsn(final int opcode, final String type) {
        Type t = Type.getObjectType(type);
        switch (opcode) {
            case org.springframework.asm.Opcodes.NEW:
                anew(t);
                break;
            case org.springframework.asm.Opcodes.ANEWARRAY:
                newarray(t);
                break;
            case org.springframework.asm.Opcodes.CHECKCAST:
                checkcast(t);
                break;
            case org.springframework.asm.Opcodes.INSTANCEOF:
                instanceOf(t);
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitFieldInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        switch (opcode) {
            case org.springframework.asm.Opcodes.GETSTATIC:
                getstatic(owner, name, desc);
                break;
            case org.springframework.asm.Opcodes.PUTSTATIC:
                putstatic(owner, name, desc);
                break;
            case org.springframework.asm.Opcodes.GETFIELD:
                getfield(owner, name, desc);
                break;
            case org.springframework.asm.Opcodes.PUTFIELD:
                putfield(owner, name, desc);
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitMethodInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        switch (opcode) {
            case org.springframework.asm.Opcodes.INVOKESPECIAL:
                invokespecial(owner, name, desc);
                break;
            case org.springframework.asm.Opcodes.INVOKEVIRTUAL:
                invokevirtual(owner, name, desc);
                break;
            case org.springframework.asm.Opcodes.INVOKESTATIC:
                invokestatic(owner, name, desc);
                break;
            case org.springframework.asm.Opcodes.INVOKEINTERFACE:
                invokeinterface(owner, name, desc);
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitInvokeDynamicInsn(
        String name,
        String desc,
        Handle bsm,
        Object... bsmArgs)
    {
       invokedynamic(name, desc, bsm, bsmArgs);
    }

    @Override
    public void visitJumpInsn(final int opcode, final Label label) {
        switch (opcode) {
            case org.springframework.asm.Opcodes.IFEQ:
                ifeq(label);
                break;
            case org.springframework.asm.Opcodes.IFNE:
                ifne(label);
                break;
            case org.springframework.asm.Opcodes.IFLT:
                iflt(label);
                break;
            case org.springframework.asm.Opcodes.IFGE:
                ifge(label);
                break;
            case org.springframework.asm.Opcodes.IFGT:
                ifgt(label);
                break;
            case org.springframework.asm.Opcodes.IFLE:
                ifle(label);
                break;
            case org.springframework.asm.Opcodes.IF_ICMPEQ:
                ificmpeq(label);
                break;
            case org.springframework.asm.Opcodes.IF_ICMPNE:
                ificmpne(label);
                break;
            case org.springframework.asm.Opcodes.IF_ICMPLT:
                ificmplt(label);
                break;
            case org.springframework.asm.Opcodes.IF_ICMPGE:
                ificmpge(label);
                break;
            case org.springframework.asm.Opcodes.IF_ICMPGT:
                ificmpgt(label);
                break;
            case org.springframework.asm.Opcodes.IF_ICMPLE:
                ificmple(label);
                break;
            case org.springframework.asm.Opcodes.IF_ACMPEQ:
                ifacmpeq(label);
                break;
            case org.springframework.asm.Opcodes.IF_ACMPNE:
                ifacmpne(label);
                break;
            case org.springframework.asm.Opcodes.GOTO:
                goTo(label);
                break;
            case org.springframework.asm.Opcodes.JSR:
                jsr(label);
                break;
            case org.springframework.asm.Opcodes.IFNULL:
                ifnull(label);
                break;
            case org.springframework.asm.Opcodes.IFNONNULL:
                ifnonnull(label);
                break;
            default:
                throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitLabel(final Label label) {
        mark(label);
    }

    @Override
    public void visitLdcInsn(final Object cst) {
        if (cst instanceof Integer) {
            int val = ((Integer) cst).intValue();
            iconst(val);
        } else if (cst instanceof Byte) {
            int val = ((Byte) cst).intValue();
            iconst(val);
        } else if (cst instanceof Character) {
            int val = ((Character) cst).charValue();
            iconst(val);
        } else if (cst instanceof Short) {
            int val = ((Short) cst).intValue();
            iconst(val);
        } else if (cst instanceof Boolean) {
            int val = ((Boolean) cst).booleanValue() ? 1 : 0;
            iconst(val);
        } else if (cst instanceof Float) {
            float val = ((Float) cst).floatValue();
            fconst(val);
        } else if (cst instanceof Long) {
            long val = ((Long) cst).longValue();
            lconst(val);
        } else if (cst instanceof Double) {
            double val = ((Double) cst).doubleValue();
            dconst(val);
        } else if (cst instanceof String) {
            aconst(cst);
        } else if (cst instanceof Type) {
            tconst((Type) cst);
        } else if (cst instanceof Handle) {
            hconst((Handle) cst);
        } else {
            throw new IllegalArgumentException();
        }
    }

    @Override
    public void visitIincInsn(final int var, final int increment) {
        iinc(var, increment);
    }

    @Override
    public void visitTableSwitchInsn(
        final int min,
        final int max,
        final Label dflt,
        final Label... labels)
    {
        tableswitch(min, max, dflt, labels);
    }

    @Override
    public void visitLookupSwitchInsn(
        final Label dflt,
        final int[] keys,
        final Label[] labels)
    {
        lookupswitch(dflt, keys, labels);
    }

    @Override
    public void visitMultiANewArrayInsn(final String desc, final int dims) {
        multianewarray(desc, dims);
    }

    // -----------------------------------------------------------------------

    public void nop() {
        mv.visitInsn(org.springframework.asm.Opcodes.NOP);
    }

    public void aconst(final Object cst) {
        if (cst == null) {
            mv.visitInsn(org.springframework.asm.Opcodes.ACONST_NULL);
        } else {
            mv.visitLdcInsn(cst);
        }
    }

    public void iconst(final int cst) {
        if (cst >= -1 && cst <= 5) {
            mv.visitInsn(org.springframework.asm.Opcodes.ICONST_0 + cst);
        } else if (cst >= Byte.MIN_VALUE && cst <= Byte.MAX_VALUE) {
            mv.visitIntInsn(org.springframework.asm.Opcodes.BIPUSH, cst);
        } else if (cst >= Short.MIN_VALUE && cst <= Short.MAX_VALUE) {
            mv.visitIntInsn(org.springframework.asm.Opcodes.SIPUSH, cst);
        } else {
            mv.visitLdcInsn(new Integer(cst));
        }
    }

    public void lconst(final long cst) {
        if (cst == 0L || cst == 1L) {
            mv.visitInsn(org.springframework.asm.Opcodes.LCONST_0 + (int) cst);
        } else {
            mv.visitLdcInsn(new Long(cst));
        }
    }

    public void fconst(final float cst) {
        int bits = Float.floatToIntBits(cst);
        if (bits == 0L || bits == 0x3f800000 || bits == 0x40000000) { // 0..2
            mv.visitInsn(org.springframework.asm.Opcodes.FCONST_0 + (int) cst);
        } else {
            mv.visitLdcInsn(new Float(cst));
        }
    }

    public void dconst(final double cst) {
        long bits = Double.doubleToLongBits(cst);
        if (bits == 0L || bits == 0x3ff0000000000000L) { // +0.0d and 1.0d
            mv.visitInsn(org.springframework.asm.Opcodes.DCONST_0 + (int) cst);
        } else {
            mv.visitLdcInsn(new Double(cst));
        }
    }

    public void tconst(final Type type) {
        mv.visitLdcInsn(type);
    }

    public void hconst(final Handle handle) {
        mv.visitLdcInsn(handle);
    }

    public void load(final int var, final Type type) {
        mv.visitVarInsn(type.getOpcode(org.springframework.asm.Opcodes.ILOAD), var);
    }

    public void aload(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IALOAD));
    }

    public void store(final int var, final Type type) {
        mv.visitVarInsn(type.getOpcode(org.springframework.asm.Opcodes.ISTORE), var);
    }

    public void astore(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IASTORE));
    }

    public void pop() {
        mv.visitInsn(org.springframework.asm.Opcodes.POP);
    }

    public void pop2() {
        mv.visitInsn(org.springframework.asm.Opcodes.POP2);
    }

    public void dup() {
        mv.visitInsn(org.springframework.asm.Opcodes.DUP);
    }

    public void dup2() {
        mv.visitInsn(org.springframework.asm.Opcodes.DUP2);
    }

    public void dupX1() {
        mv.visitInsn(org.springframework.asm.Opcodes.DUP_X1);
    }

    public void dupX2() {
        mv.visitInsn(org.springframework.asm.Opcodes.DUP_X2);
    }

    public void dup2X1() {
        mv.visitInsn(org.springframework.asm.Opcodes.DUP2_X1);
    }

    public void dup2X2() {
        mv.visitInsn(org.springframework.asm.Opcodes.DUP2_X2);
    }

    public void swap() {
        mv.visitInsn(org.springframework.asm.Opcodes.SWAP);
    }

    public void add(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IADD));
    }

    public void sub(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.ISUB));
    }

    public void mul(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IMUL));
    }

    public void div(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IDIV));
    }

    public void rem(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IREM));
    }

    public void neg(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.INEG));
    }

    public void shl(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.ISHL));
    }

    public void shr(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.ISHR));
    }

    public void ushr(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IUSHR));
    }

    public void and(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IAND));
    }

    public void or(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IOR));
    }

    public void xor(final Type type) {
        mv.visitInsn(type.getOpcode(org.springframework.asm.Opcodes.IXOR));
    }

    public void iinc(final int var, final int increment) {
        mv.visitIincInsn(var, increment);
    }

    public void cast(final Type from, final Type to) {
        if (from != to) {
            if (from == Type.DOUBLE_TYPE) {
                if (to == Type.FLOAT_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.D2F);
                } else if (to == Type.LONG_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.D2L);
                } else {
                    mv.visitInsn(org.springframework.asm.Opcodes.D2I);
                    cast(Type.INT_TYPE, to);
                }
            } else if (from == Type.FLOAT_TYPE) {
                if (to == Type.DOUBLE_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.F2D);
                } else if (to == Type.LONG_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.F2L);
                } else {
                    mv.visitInsn(org.springframework.asm.Opcodes.F2I);
                    cast(Type.INT_TYPE, to);
                }
            } else if (from == Type.LONG_TYPE) {
                if (to == Type.DOUBLE_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.L2D);
                } else if (to == Type.FLOAT_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.L2F);
                } else {
                    mv.visitInsn(org.springframework.asm.Opcodes.L2I);
                    cast(Type.INT_TYPE, to);
                }
            } else {
                if (to == Type.BYTE_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.I2B);
                } else if (to == Type.CHAR_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.I2C);
                } else if (to == Type.DOUBLE_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.I2D);
                } else if (to == Type.FLOAT_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.I2F);
                } else if (to == Type.LONG_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.I2L);
                } else if (to == Type.SHORT_TYPE) {
                    mv.visitInsn(org.springframework.asm.Opcodes.I2S);
                }
            }
        }
    }

    public void lcmp() {
        mv.visitInsn(org.springframework.asm.Opcodes.LCMP);
    }

    public void cmpl(final Type type) {
        mv.visitInsn(type == Type.FLOAT_TYPE ? org.springframework.asm.Opcodes.FCMPL : org.springframework.asm.Opcodes.DCMPL);
    }

    public void cmpg(final Type type) {
        mv.visitInsn(type == Type.FLOAT_TYPE ? org.springframework.asm.Opcodes.FCMPG : org.springframework.asm.Opcodes.DCMPG);
    }

    public void ifeq(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFEQ, label);
    }

    public void ifne(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFNE, label);
    }

    public void iflt(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFLT, label);
    }

    public void ifge(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFGE, label);
    }

    public void ifgt(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFGT, label);
    }

    public void ifle(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFLE, label);
    }

    public void ificmpeq(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ICMPEQ, label);
    }

    public void ificmpne(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ICMPNE, label);
    }

    public void ificmplt(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ICMPLT, label);
    }

    public void ificmpge(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ICMPGE, label);
    }

    public void ificmpgt(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ICMPGT, label);
    }

    public void ificmple(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ICMPLE, label);
    }

    public void ifacmpeq(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ACMPEQ, label);
    }

    public void ifacmpne(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IF_ACMPNE, label);
    }

    public void goTo(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.GOTO, label);
    }

    public void jsr(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.JSR, label);
    }

    public void ret(final int var) {
        mv.visitVarInsn(org.springframework.asm.Opcodes.RET, var);
    }

    public void tableswitch(
        final int min,
        final int max,
        final Label dflt,
        final Label... labels)
    {
        mv.visitTableSwitchInsn(min, max, dflt, labels);
    }

    public void lookupswitch(
        final Label dflt,
        final int[] keys,
        final Label[] labels)
    {
        mv.visitLookupSwitchInsn(dflt, keys, labels);
    }

    public void areturn(final Type t) {
        mv.visitInsn(t.getOpcode(org.springframework.asm.Opcodes.IRETURN));
    }

    public void getstatic(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitFieldInsn(org.springframework.asm.Opcodes.GETSTATIC, owner, name, desc);
    }

    public void putstatic(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitFieldInsn(org.springframework.asm.Opcodes.PUTSTATIC, owner, name, desc);
    }

    public void getfield(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitFieldInsn(org.springframework.asm.Opcodes.GETFIELD, owner, name, desc);
    }

    public void putfield(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitFieldInsn(org.springframework.asm.Opcodes.PUTFIELD, owner, name, desc);
    }

    public void invokevirtual(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitMethodInsn(org.springframework.asm.Opcodes.INVOKEVIRTUAL, owner, name, desc);
    }

    public void invokespecial(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitMethodInsn(org.springframework.asm.Opcodes.INVOKESPECIAL, owner, name, desc);
    }

    public void invokestatic(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitMethodInsn(org.springframework.asm.Opcodes.INVOKESTATIC, owner, name, desc);
    }

    public void invokeinterface(
        final String owner,
        final String name,
        final String desc)
    {
        mv.visitMethodInsn(org.springframework.asm.Opcodes.INVOKEINTERFACE, owner, name, desc);
    }

    public void invokedynamic(
        String name,
        String desc,
        Handle bsm,
        Object[] bsmArgs)
    {
        mv.visitInvokeDynamicInsn(name, desc, bsm, bsmArgs);
    }

    public void anew(final Type type) {
        mv.visitTypeInsn(org.springframework.asm.Opcodes.NEW, type.getInternalName());
    }

    public void newarray(final Type type) {
        int typ;
        switch (type.getSort()) {
            case Type.BOOLEAN:
                typ = org.springframework.asm.Opcodes.T_BOOLEAN;
                break;
            case Type.CHAR:
                typ = org.springframework.asm.Opcodes.T_CHAR;
                break;
            case Type.BYTE:
                typ = org.springframework.asm.Opcodes.T_BYTE;
                break;
            case Type.SHORT:
                typ = org.springframework.asm.Opcodes.T_SHORT;
                break;
            case Type.INT:
                typ = org.springframework.asm.Opcodes.T_INT;
                break;
            case Type.FLOAT:
                typ = org.springframework.asm.Opcodes.T_FLOAT;
                break;
            case Type.LONG:
                typ = org.springframework.asm.Opcodes.T_LONG;
                break;
            case Type.DOUBLE:
                typ = org.springframework.asm.Opcodes.T_DOUBLE;
                break;
            default:
                mv.visitTypeInsn(org.springframework.asm.Opcodes.ANEWARRAY, type.getInternalName());
                return;
        }
        mv.visitIntInsn(org.springframework.asm.Opcodes.NEWARRAY, typ);
    }

    public void arraylength() {
        mv.visitInsn(org.springframework.asm.Opcodes.ARRAYLENGTH);
    }

    public void athrow() {
        mv.visitInsn(org.springframework.asm.Opcodes.ATHROW);
    }

    public void checkcast(final Type type) {
        mv.visitTypeInsn(org.springframework.asm.Opcodes.CHECKCAST, type.getInternalName());
    }

    public void instanceOf(final Type type) {
        mv.visitTypeInsn(org.springframework.asm.Opcodes.INSTANCEOF, type.getInternalName());
    }

    public void monitorenter() {
        mv.visitInsn(org.springframework.asm.Opcodes.MONITORENTER);
    }

    public void monitorexit() {
        mv.visitInsn(org.springframework.asm.Opcodes.MONITOREXIT);
    }

    public void multianewarray(final String desc, final int dims) {
        mv.visitMultiANewArrayInsn(desc, dims);
    }

    public void ifnull(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFNULL, label);
    }

    public void ifnonnull(final Label label) {
        mv.visitJumpInsn(org.springframework.asm.Opcodes.IFNONNULL, label);
    }

    public void mark(final Label label) {
        mv.visitLabel(label);
    }
}
