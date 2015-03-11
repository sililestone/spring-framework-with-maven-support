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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.asm.Label;

/**
 * A {@link org.springframework.asm.MethodVisitor} that keeps track of stack map frame changes between
 * {@link #visitFrame(int, int, Object[], int, Object[]) visitFrame} calls. This
 * adapter must be used with the
 * {@link org.springframework.asm.ClassReader#EXPAND_FRAMES} option. Each visit<i>X</i>
 * instruction delegates to the next visitor in the chain, if any, and then
 * simulates the effect of this instruction on the stack map frame, represented
 * by {@link #locals} and {@link #stack}. The next visitor in the chain can get
 * the state of the stack map frame <i>before</i> each instruction by reading
 * the value of these fields in its visit<i>X</i> methods (this requires a
 * reference to the AnalyzerAdapter that is before it in the chain).
 * If this adapter is used with a class that does not contain stack map table
 * attributes (i.e., pre Java 6 classes) then this adapter may not be able to
 * compute the stack map frame for each instruction. In this case no exception
 * is thrown but the {@link #locals} and {@link #stack} fields will be null for
 * these instructions.
 *
 * @author Eric Bruneton
 */
public class AnalyzerAdapter extends org.springframework.asm.MethodVisitor {

    /**
     * <code>List</code> of the local variable slots for current execution
     * frame. Primitive types are represented by {@link org.springframework.asm.Opcodes#TOP},
     * {@link org.springframework.asm.Opcodes#INTEGER}, {@link org.springframework.asm.Opcodes#FLOAT}, {@link org.springframework.asm.Opcodes#LONG},
     * {@link org.springframework.asm.Opcodes#DOUBLE},{@link org.springframework.asm.Opcodes#NULL} or
     * {@link org.springframework.asm.Opcodes#UNINITIALIZED_THIS} (long and double are represented by a
     * two elements, the second one being TOP). Reference types are represented
     * by String objects (representing internal names), and uninitialized types
     * by Label objects (this label designates the NEW instruction that created
     * this uninitialized value). This field is <tt>null</tt> for unreacheable
     * instructions.
     */
    public List<Object> locals;

    /**
     * <code>List</code> of the operand stack slots for current execution
     * frame. Primitive types are represented by {@link org.springframework.asm.Opcodes#TOP},
     * {@link org.springframework.asm.Opcodes#INTEGER}, {@link org.springframework.asm.Opcodes#FLOAT}, {@link org.springframework.asm.Opcodes#LONG},
     * {@link org.springframework.asm.Opcodes#DOUBLE},{@link org.springframework.asm.Opcodes#NULL} or
     * {@link org.springframework.asm.Opcodes#UNINITIALIZED_THIS} (long and double are represented by a
     * two elements, the second one being TOP). Reference types are represented
     * by String objects (representing internal names), and uninitialized types
     * by Label objects (this label designates the NEW instruction that created
     * this uninitialized value). This field is <tt>null</tt> for unreacheable
     * instructions.
     */
    public List<Object> stack;

    /**
     * The labels that designate the next instruction to be visited. May be
     * <tt>null</tt>.
     */
    private List<Label> labels;

    /**
     * Information about uninitialized types in the current execution frame.
     * This map associates internal names to Label objects. Each label
     * designates a NEW instruction that created the currently uninitialized
     * types, and the associated internal name represents the NEW operand, i.e.
     * the final, initialized type value.
     */
    public Map<Object,Object> uninitializedTypes;

    /**
     * The maximum stack size of this method.
     */
    private int maxStack;

    /**
     * The maximum number of local variables of this method.
     */
    private int maxLocals;

    /**
     * The owner's class name.
     */
    private String owner;

    /**
     * Creates a new {@link AnalyzerAdapter}. <i>Subclasses must not use this
     * constructor</i>. Instead, they must use the
     * {@link #AnalyzerAdapter(int, String, int, String, String, org.springframework.asm.MethodVisitor)}
     * version.
     *
     * @param owner the owner's class name.
     * @param access the method's access flags (see {@link org.springframework.asm.Opcodes}).
     * @param name the method's name.
     * @param desc the method's descriptor (see {@link org.springframework.asm.Type Type}).
     * @param mv the method visitor to which this adapter delegates calls. May
     *        be <tt>null</tt>.
     */
    public AnalyzerAdapter(
        final String owner,
        final int access,
        final String name,
        final String desc,
        final org.springframework.asm.MethodVisitor mv)
    {
        this(org.springframework.asm.Opcodes.ASM4, owner, access, name, desc, mv);
    }

    /**
     * Creates a new {@link AnalyzerAdapter}.
     *
     * @param api the ASM API version implemented by this visitor. Must be one
     *        of {@link org.springframework.asm.Opcodes#ASM4}.
     * @param owner the owner's class name.
     * @param access the method's access flags (see {@link org.springframework.asm.Opcodes}).
     * @param name the method's name.
     * @param desc the method's descriptor (see {@link org.springframework.asm.Type Type}).
     * @param mv the method visitor to which this adapter delegates calls. May
     *        be <tt>null</tt>.
     */
    protected AnalyzerAdapter(
        final int api,
        final String owner,
        final int access,
        final String name,
        final String desc,
        final org.springframework.asm.MethodVisitor mv)
    {
        super(api, mv);
        this.owner = owner;
        locals = new ArrayList<Object>();
        stack = new ArrayList<Object>();
        uninitializedTypes = new HashMap<Object, Object>();

        if ((access & org.springframework.asm.Opcodes.ACC_STATIC) == 0) {
            if ("<init>".equals(name)) {
                locals.add(org.springframework.asm.Opcodes.UNINITIALIZED_THIS);
            } else {
                locals.add(owner);
            }
        }
        org.springframework.asm.Type[] types = org.springframework.asm.Type.getArgumentTypes(desc);
        for (int i = 0; i < types.length; ++i) {
            org.springframework.asm.Type type = types[i];
            switch (type.getSort()) {
                case org.springframework.asm.Type.BOOLEAN:
                case org.springframework.asm.Type.CHAR:
                case org.springframework.asm.Type.BYTE:
                case org.springframework.asm.Type.SHORT:
                case org.springframework.asm.Type.INT:
                    locals.add(org.springframework.asm.Opcodes.INTEGER);
                    break;
                case org.springframework.asm.Type.FLOAT:
                    locals.add(org.springframework.asm.Opcodes.FLOAT);
                    break;
                case org.springframework.asm.Type.LONG:
                    locals.add(org.springframework.asm.Opcodes.LONG);
                    locals.add(org.springframework.asm.Opcodes.TOP);
                    break;
                case org.springframework.asm.Type.DOUBLE:
                    locals.add(org.springframework.asm.Opcodes.DOUBLE);
                    locals.add(org.springframework.asm.Opcodes.TOP);
                    break;
                case org.springframework.asm.Type.ARRAY:
                    locals.add(types[i].getDescriptor());
                    break;
                // case Type.OBJECT:
                default:
                    locals.add(types[i].getInternalName());
            }
        }
    }

    @Override
    public void visitFrame(
        final int type,
        final int nLocal,
        final Object[] local,
        final int nStack,
        final Object[] stack)
    {
        if (type != org.springframework.asm.Opcodes.F_NEW) { // uncompressed frame
            throw new IllegalStateException("ClassReader.accept() should be called with EXPAND_FRAMES flag");
        }

        if (mv != null) {
            mv.visitFrame(type, nLocal, local, nStack, stack);
        }

        if (this.locals != null) {
            this.locals.clear();
            this.stack.clear();
        } else {
            this.locals = new ArrayList<Object>();
            this.stack = new ArrayList<Object>();
        }
        visitFrameTypes(nLocal, local, this.locals);
        visitFrameTypes(nStack, stack, this.stack);
        maxStack = Math.max(maxStack, this.stack.size());
    }

    private static void visitFrameTypes(
        final int n,
        final Object[] types,
        final List<Object> result)
    {
        for (int i = 0; i < n; ++i) {
            Object type = types[i];
            result.add(type);
            if (type == org.springframework.asm.Opcodes.LONG || type == org.springframework.asm.Opcodes.DOUBLE) {
                result.add(org.springframework.asm.Opcodes.TOP);
            }
        }
    }

    @Override
    public void visitInsn(final int opcode) {
        if (mv != null) {
            mv.visitInsn(opcode);
        }
        execute(opcode, 0, null);
        if ((opcode >= org.springframework.asm.Opcodes.IRETURN && opcode <= org.springframework.asm.Opcodes.RETURN)
                || opcode == org.springframework.asm.Opcodes.ATHROW)
        {
            this.locals = null;
            this.stack = null;
        }
    }

    @Override
    public void visitIntInsn(final int opcode, final int operand) {
        if (mv != null) {
            mv.visitIntInsn(opcode, operand);
        }
        execute(opcode, operand, null);
    }

    @Override
    public void visitVarInsn(final int opcode, final int var) {
        if (mv != null) {
            mv.visitVarInsn(opcode, var);
        }
        execute(opcode, var, null);
    }

    @Override
    public void visitTypeInsn(final int opcode, final String type) {
        if (opcode == org.springframework.asm.Opcodes.NEW) {
            if (labels == null) {
                Label l = new Label();
                labels = new ArrayList<Label>(3);
                labels.add(l);
                if (mv != null) {
                    mv.visitLabel(l);
                }
            }
            for (int i = 0; i < labels.size(); ++i) {
                uninitializedTypes.put(labels.get(i), type);
            }
        }
        if (mv != null) {
            mv.visitTypeInsn(opcode, type);
        }
        execute(opcode, 0, type);
    }

    @Override
    public void visitFieldInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        if (mv != null) {
            mv.visitFieldInsn(opcode, owner, name, desc);
        }
        execute(opcode, 0, desc);
    }

    @Override
    public void visitMethodInsn(
        final int opcode,
        final String owner,
        final String name,
        final String desc)
    {
        if (mv != null) {
            mv.visitMethodInsn(opcode, owner, name, desc);
        }
        if (this.locals == null) {
            labels = null;
            return;
        }
        pop(desc);
        if (opcode != org.springframework.asm.Opcodes.INVOKESTATIC) {
            Object t = pop();
            if (opcode == org.springframework.asm.Opcodes.INVOKESPECIAL && name.charAt(0) == '<') {
                Object u;
                if (t == org.springframework.asm.Opcodes.UNINITIALIZED_THIS) {
                    u = this.owner;
                } else {
                    u = uninitializedTypes.get(t);
                }
                for (int i = 0; i < locals.size(); ++i) {
                    if (locals.get(i) == t) {
                        locals.set(i, u);
                    }
                }
                for (int i = 0; i < stack.size(); ++i) {
                    if (stack.get(i) == t) {
                        stack.set(i, u);
                    }
                }
            }
        }
        pushDesc(desc);
        labels = null;
    }

    @Override
    public void visitInvokeDynamicInsn(
        String name,
        String desc,
        org.springframework.asm.Handle bsm,
        Object... bsmArgs)
    {
        if (mv != null) {
            mv.visitInvokeDynamicInsn(name, desc, bsm, bsmArgs);
        }
        if (this.locals == null) {
            labels = null;
            return;
        }
        pop(desc);
        pushDesc(desc);
        labels = null;
    }

    @Override
    public void visitJumpInsn(final int opcode, final Label label) {
        if (mv != null) {
            mv.visitJumpInsn(opcode, label);
        }
        execute(opcode, 0, null);
        if (opcode == org.springframework.asm.Opcodes.GOTO) {
            this.locals = null;
            this.stack = null;
        }
    }

    @Override
    public void visitLabel(final Label label) {
        if (mv != null) {
            mv.visitLabel(label);
        }
        if (labels == null) {
            labels = new ArrayList<Label>(3);
        }
        labels.add(label);
    }

    @Override
    public void visitLdcInsn(final Object cst) {
        if (mv != null) {
            mv.visitLdcInsn(cst);
        }
        if (this.locals == null) {
            labels = null;
            return;
        }
        if (cst instanceof Integer) {
            push(org.springframework.asm.Opcodes.INTEGER);
        } else if (cst instanceof Long) {
            push(org.springframework.asm.Opcodes.LONG);
            push(org.springframework.asm.Opcodes.TOP);
        } else if (cst instanceof Float) {
            push(org.springframework.asm.Opcodes.FLOAT);
        } else if (cst instanceof Double) {
            push(org.springframework.asm.Opcodes.DOUBLE);
            push(org.springframework.asm.Opcodes.TOP);
        } else if (cst instanceof String) {
            push("java/lang/String");
        } else if (cst instanceof org.springframework.asm.Type) {
            int sort = ((org.springframework.asm.Type) cst).getSort();
            if (sort == org.springframework.asm.Type.OBJECT || sort == org.springframework.asm.Type.ARRAY) {
                push("java/lang/Class");
            } else if (sort == org.springframework.asm.Type.METHOD) {
                push("java/lang/invoke/MethodType");
            } else {
                throw new IllegalArgumentException();
            }
        } else if (cst instanceof org.springframework.asm.Handle) {
            push("java/lang/invoke/MethodHandle");
        } else {
            throw new IllegalArgumentException();
        }
        labels = null;
    }

    @Override
    public void visitIincInsn(final int var, final int increment) {
        if (mv != null) {
            mv.visitIincInsn(var, increment);
        }
        execute(org.springframework.asm.Opcodes.IINC, var, null);
    }

    @Override
    public void visitTableSwitchInsn(
        final int min,
        final int max,
        final Label dflt,
        final Label... labels)
    {
        if (mv != null) {
            mv.visitTableSwitchInsn(min, max, dflt, labels);
        }
        execute(org.springframework.asm.Opcodes.TABLESWITCH, 0, null);
        this.locals = null;
        this.stack = null;
    }

    @Override
    public void visitLookupSwitchInsn(
        final Label dflt,
        final int[] keys,
        final Label[] labels)
    {
        if (mv != null) {
            mv.visitLookupSwitchInsn(dflt, keys, labels);
        }
        execute(org.springframework.asm.Opcodes.LOOKUPSWITCH, 0, null);
        this.locals = null;
        this.stack = null;
    }

    @Override
    public void visitMultiANewArrayInsn(final String desc, final int dims) {
        if (mv != null) {
            mv.visitMultiANewArrayInsn(desc, dims);
        }
        execute(org.springframework.asm.Opcodes.MULTIANEWARRAY, dims, desc);
    }

    @Override
    public void visitMaxs(final int maxStack, final int maxLocals) {
        if (mv != null) {
            this.maxStack = Math.max(this.maxStack, maxStack);
            this.maxLocals = Math.max(this.maxLocals, maxLocals);
            mv.visitMaxs(this.maxStack, this.maxLocals);
        }
    }

    // ------------------------------------------------------------------------

    private Object get(final int local) {
        maxLocals = Math.max(maxLocals, local);
        return local < locals.size() ? locals.get(local) : org.springframework.asm.Opcodes.TOP;
    }

    private void set(final int local, final Object type) {
        maxLocals = Math.max(maxLocals, local);
        while (local >= locals.size()) {
            locals.add(org.springframework.asm.Opcodes.TOP);
        }
        locals.set(local, type);
    }

    private void push(final Object type) {
        stack.add(type);
        maxStack = Math.max(maxStack, stack.size());
    }

    private void pushDesc(final String desc) {
        int index = desc.charAt(0) == '(' ? desc.indexOf(')') + 1 : 0;
        switch (desc.charAt(index)) {
            case 'V':
                return;
            case 'Z':
            case 'C':
            case 'B':
            case 'S':
            case 'I':
                push(org.springframework.asm.Opcodes.INTEGER);
                return;
            case 'F':
                push(org.springframework.asm.Opcodes.FLOAT);
                return;
            case 'J':
                push(org.springframework.asm.Opcodes.LONG);
                push(org.springframework.asm.Opcodes.TOP);
                return;
            case 'D':
                push(org.springframework.asm.Opcodes.DOUBLE);
                push(org.springframework.asm.Opcodes.TOP);
                return;
            case '[':
                if (index == 0) {
                    push(desc);
                } else {
                    push(desc.substring(index, desc.length()));
                }
                break;
            // case 'L':
            default:
                if (index == 0) {
                    push(desc.substring(1, desc.length() - 1));
                } else {
                    push(desc.substring(index + 1, desc.length() - 1));
                }
        }
    }

    private Object pop() {
        return stack.remove(stack.size() - 1);
    }

    private void pop(final int n) {
        int size = stack.size();
        int end = size - n;
        for (int i = size - 1; i >= end; --i) {
            stack.remove(i);
        }
    }

    private void pop(final String desc) {
        char c = desc.charAt(0);
        if (c == '(') {
            int n = 0;
            org.springframework.asm.Type[] types = org.springframework.asm.Type.getArgumentTypes(desc);
            for (int i = 0; i < types.length; ++i) {
                n += types[i].getSize();
            }
            pop(n);
        } else if (c == 'J' || c == 'D') {
            pop(2);
        } else {
            pop(1);
        }
    }

    private void execute(final int opcode, final int iarg, final String sarg) {
        if (this.locals == null) {
            labels = null;
            return;
        }
        Object t1, t2, t3, t4;
        switch (opcode) {
            case org.springframework.asm.Opcodes.NOP:
            case org.springframework.asm.Opcodes.INEG:
            case org.springframework.asm.Opcodes.LNEG:
            case org.springframework.asm.Opcodes.FNEG:
            case org.springframework.asm.Opcodes.DNEG:
            case org.springframework.asm.Opcodes.I2B:
            case org.springframework.asm.Opcodes.I2C:
            case org.springframework.asm.Opcodes.I2S:
            case org.springframework.asm.Opcodes.GOTO:
            case org.springframework.asm.Opcodes.RETURN:
                break;
            case org.springframework.asm.Opcodes.ACONST_NULL:
                push(org.springframework.asm.Opcodes.NULL);
                break;
            case org.springframework.asm.Opcodes.ICONST_M1:
            case org.springframework.asm.Opcodes.ICONST_0:
            case org.springframework.asm.Opcodes.ICONST_1:
            case org.springframework.asm.Opcodes.ICONST_2:
            case org.springframework.asm.Opcodes.ICONST_3:
            case org.springframework.asm.Opcodes.ICONST_4:
            case org.springframework.asm.Opcodes.ICONST_5:
            case org.springframework.asm.Opcodes.BIPUSH:
            case org.springframework.asm.Opcodes.SIPUSH:
                push(org.springframework.asm.Opcodes.INTEGER);
                break;
            case org.springframework.asm.Opcodes.LCONST_0:
            case org.springframework.asm.Opcodes.LCONST_1:
                push(org.springframework.asm.Opcodes.LONG);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.FCONST_0:
            case org.springframework.asm.Opcodes.FCONST_1:
            case org.springframework.asm.Opcodes.FCONST_2:
                push(org.springframework.asm.Opcodes.FLOAT);
                break;
            case org.springframework.asm.Opcodes.DCONST_0:
            case org.springframework.asm.Opcodes.DCONST_1:
                push(org.springframework.asm.Opcodes.DOUBLE);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.ILOAD:
            case org.springframework.asm.Opcodes.FLOAD:
            case org.springframework.asm.Opcodes.ALOAD:
                push(get(iarg));
                break;
            case org.springframework.asm.Opcodes.LLOAD:
            case org.springframework.asm.Opcodes.DLOAD:
                push(get(iarg));
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.IALOAD:
            case org.springframework.asm.Opcodes.BALOAD:
            case org.springframework.asm.Opcodes.CALOAD:
            case org.springframework.asm.Opcodes.SALOAD:
                pop(2);
                push(org.springframework.asm.Opcodes.INTEGER);
                break;
            case org.springframework.asm.Opcodes.LALOAD:
            case org.springframework.asm.Opcodes.D2L:
                pop(2);
                push(org.springframework.asm.Opcodes.LONG);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.FALOAD:
                pop(2);
                push(org.springframework.asm.Opcodes.FLOAT);
                break;
            case org.springframework.asm.Opcodes.DALOAD:
            case org.springframework.asm.Opcodes.L2D:
                pop(2);
                push(org.springframework.asm.Opcodes.DOUBLE);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.AALOAD:
                pop(1);
                t1 = pop();
                if (t1 instanceof String) {
                    pushDesc(((String) t1).substring(1));
                } else {
                    push("java/lang/Object");
                }
                break;
            case org.springframework.asm.Opcodes.ISTORE:
            case org.springframework.asm.Opcodes.FSTORE:
            case org.springframework.asm.Opcodes.ASTORE:
                t1 = pop();
                set(iarg, t1);
                if (iarg > 0) {
                    t2 = get(iarg - 1);
                    if (t2 == org.springframework.asm.Opcodes.LONG || t2 == org.springframework.asm.Opcodes.DOUBLE) {
                        set(iarg - 1, org.springframework.asm.Opcodes.TOP);
                    }
                }
                break;
            case org.springframework.asm.Opcodes.LSTORE:
            case org.springframework.asm.Opcodes.DSTORE:
                pop(1);
                t1 = pop();
                set(iarg, t1);
                set(iarg + 1, org.springframework.asm.Opcodes.TOP);
                if (iarg > 0) {
                    t2 = get(iarg - 1);
                    if (t2 == org.springframework.asm.Opcodes.LONG || t2 == org.springframework.asm.Opcodes.DOUBLE) {
                        set(iarg - 1, org.springframework.asm.Opcodes.TOP);
                    }
                }
                break;
            case org.springframework.asm.Opcodes.IASTORE:
            case org.springframework.asm.Opcodes.BASTORE:
            case org.springframework.asm.Opcodes.CASTORE:
            case org.springframework.asm.Opcodes.SASTORE:
            case org.springframework.asm.Opcodes.FASTORE:
            case org.springframework.asm.Opcodes.AASTORE:
                pop(3);
                break;
            case org.springframework.asm.Opcodes.LASTORE:
            case org.springframework.asm.Opcodes.DASTORE:
                pop(4);
                break;
            case org.springframework.asm.Opcodes.POP:
            case org.springframework.asm.Opcodes.IFEQ:
            case org.springframework.asm.Opcodes.IFNE:
            case org.springframework.asm.Opcodes.IFLT:
            case org.springframework.asm.Opcodes.IFGE:
            case org.springframework.asm.Opcodes.IFGT:
            case org.springframework.asm.Opcodes.IFLE:
            case org.springframework.asm.Opcodes.IRETURN:
            case org.springframework.asm.Opcodes.FRETURN:
            case org.springframework.asm.Opcodes.ARETURN:
            case org.springframework.asm.Opcodes.TABLESWITCH:
            case org.springframework.asm.Opcodes.LOOKUPSWITCH:
            case org.springframework.asm.Opcodes.ATHROW:
            case org.springframework.asm.Opcodes.MONITORENTER:
            case org.springframework.asm.Opcodes.MONITOREXIT:
            case org.springframework.asm.Opcodes.IFNULL:
            case org.springframework.asm.Opcodes.IFNONNULL:
                pop(1);
                break;
            case org.springframework.asm.Opcodes.POP2:
            case org.springframework.asm.Opcodes.IF_ICMPEQ:
            case org.springframework.asm.Opcodes.IF_ICMPNE:
            case org.springframework.asm.Opcodes.IF_ICMPLT:
            case org.springframework.asm.Opcodes.IF_ICMPGE:
            case org.springframework.asm.Opcodes.IF_ICMPGT:
            case org.springframework.asm.Opcodes.IF_ICMPLE:
            case org.springframework.asm.Opcodes.IF_ACMPEQ:
            case org.springframework.asm.Opcodes.IF_ACMPNE:
            case org.springframework.asm.Opcodes.LRETURN:
            case org.springframework.asm.Opcodes.DRETURN:
                pop(2);
                break;
            case org.springframework.asm.Opcodes.DUP:
                t1 = pop();
                push(t1);
                push(t1);
                break;
            case org.springframework.asm.Opcodes.DUP_X1:
                t1 = pop();
                t2 = pop();
                push(t1);
                push(t2);
                push(t1);
                break;
            case org.springframework.asm.Opcodes.DUP_X2:
                t1 = pop();
                t2 = pop();
                t3 = pop();
                push(t1);
                push(t3);
                push(t2);
                push(t1);
                break;
            case org.springframework.asm.Opcodes.DUP2:
                t1 = pop();
                t2 = pop();
                push(t2);
                push(t1);
                push(t2);
                push(t1);
                break;
            case org.springframework.asm.Opcodes.DUP2_X1:
                t1 = pop();
                t2 = pop();
                t3 = pop();
                push(t2);
                push(t1);
                push(t3);
                push(t2);
                push(t1);
                break;
            case org.springframework.asm.Opcodes.DUP2_X2:
                t1 = pop();
                t2 = pop();
                t3 = pop();
                t4 = pop();
                push(t2);
                push(t1);
                push(t4);
                push(t3);
                push(t2);
                push(t1);
                break;
            case org.springframework.asm.Opcodes.SWAP:
                t1 = pop();
                t2 = pop();
                push(t1);
                push(t2);
                break;
            case org.springframework.asm.Opcodes.IADD:
            case org.springframework.asm.Opcodes.ISUB:
            case org.springframework.asm.Opcodes.IMUL:
            case org.springframework.asm.Opcodes.IDIV:
            case org.springframework.asm.Opcodes.IREM:
            case org.springframework.asm.Opcodes.IAND:
            case org.springframework.asm.Opcodes.IOR:
            case org.springframework.asm.Opcodes.IXOR:
            case org.springframework.asm.Opcodes.ISHL:
            case org.springframework.asm.Opcodes.ISHR:
            case org.springframework.asm.Opcodes.IUSHR:
            case org.springframework.asm.Opcodes.L2I:
            case org.springframework.asm.Opcodes.D2I:
            case org.springframework.asm.Opcodes.FCMPL:
            case org.springframework.asm.Opcodes.FCMPG:
                pop(2);
                push(org.springframework.asm.Opcodes.INTEGER);
                break;
            case org.springframework.asm.Opcodes.LADD:
            case org.springframework.asm.Opcodes.LSUB:
            case org.springframework.asm.Opcodes.LMUL:
            case org.springframework.asm.Opcodes.LDIV:
            case org.springframework.asm.Opcodes.LREM:
            case org.springframework.asm.Opcodes.LAND:
            case org.springframework.asm.Opcodes.LOR:
            case org.springframework.asm.Opcodes.LXOR:
                pop(4);
                push(org.springframework.asm.Opcodes.LONG);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.FADD:
            case org.springframework.asm.Opcodes.FSUB:
            case org.springframework.asm.Opcodes.FMUL:
            case org.springframework.asm.Opcodes.FDIV:
            case org.springframework.asm.Opcodes.FREM:
            case org.springframework.asm.Opcodes.L2F:
            case org.springframework.asm.Opcodes.D2F:
                pop(2);
                push(org.springframework.asm.Opcodes.FLOAT);
                break;
            case org.springframework.asm.Opcodes.DADD:
            case org.springframework.asm.Opcodes.DSUB:
            case org.springframework.asm.Opcodes.DMUL:
            case org.springframework.asm.Opcodes.DDIV:
            case org.springframework.asm.Opcodes.DREM:
                pop(4);
                push(org.springframework.asm.Opcodes.DOUBLE);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.LSHL:
            case org.springframework.asm.Opcodes.LSHR:
            case org.springframework.asm.Opcodes.LUSHR:
                pop(3);
                push(org.springframework.asm.Opcodes.LONG);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.IINC:
                set(iarg, org.springframework.asm.Opcodes.INTEGER);
                break;
            case org.springframework.asm.Opcodes.I2L:
            case org.springframework.asm.Opcodes.F2L:
                pop(1);
                push(org.springframework.asm.Opcodes.LONG);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.I2F:
                pop(1);
                push(org.springframework.asm.Opcodes.FLOAT);
                break;
            case org.springframework.asm.Opcodes.I2D:
            case org.springframework.asm.Opcodes.F2D:
                pop(1);
                push(org.springframework.asm.Opcodes.DOUBLE);
                push(org.springframework.asm.Opcodes.TOP);
                break;
            case org.springframework.asm.Opcodes.F2I:
            case org.springframework.asm.Opcodes.ARRAYLENGTH:
            case org.springframework.asm.Opcodes.INSTANCEOF:
                pop(1);
                push(org.springframework.asm.Opcodes.INTEGER);
                break;
            case org.springframework.asm.Opcodes.LCMP:
            case org.springframework.asm.Opcodes.DCMPL:
            case org.springframework.asm.Opcodes.DCMPG:
                pop(4);
                push(org.springframework.asm.Opcodes.INTEGER);
                break;
            case org.springframework.asm.Opcodes.JSR:
            case org.springframework.asm.Opcodes.RET:
                throw new RuntimeException("JSR/RET are not supported");
            case org.springframework.asm.Opcodes.GETSTATIC:
                pushDesc(sarg);
                break;
            case org.springframework.asm.Opcodes.PUTSTATIC:
                pop(sarg);
                break;
            case org.springframework.asm.Opcodes.GETFIELD:
                pop(1);
                pushDesc(sarg);
                break;
            case org.springframework.asm.Opcodes.PUTFIELD:
                pop(sarg);
                pop();
                break;
            case org.springframework.asm.Opcodes.NEW:
                push(labels.get(0));
                break;
            case org.springframework.asm.Opcodes.NEWARRAY:
                pop();
                switch (iarg) {
                    case org.springframework.asm.Opcodes.T_BOOLEAN:
                        pushDesc("[Z");
                        break;
                    case org.springframework.asm.Opcodes.T_CHAR:
                        pushDesc("[C");
                        break;
                    case org.springframework.asm.Opcodes.T_BYTE:
                        pushDesc("[B");
                        break;
                    case org.springframework.asm.Opcodes.T_SHORT:
                        pushDesc("[S");
                        break;
                    case org.springframework.asm.Opcodes.T_INT:
                        pushDesc("[I");
                        break;
                    case org.springframework.asm.Opcodes.T_FLOAT:
                        pushDesc("[F");
                        break;
                    case org.springframework.asm.Opcodes.T_DOUBLE:
                        pushDesc("[D");
                        break;
                    // case Opcodes.T_LONG:
                    default:
                        pushDesc("[J");
                        break;
                }
                break;
            case org.springframework.asm.Opcodes.ANEWARRAY:
                pop();
                pushDesc("[" + org.springframework.asm.Type.getObjectType(sarg));
                break;
            case org.springframework.asm.Opcodes.CHECKCAST:
                pop();
                pushDesc(org.springframework.asm.Type.getObjectType(sarg).getDescriptor());
                break;
            // case Opcodes.MULTIANEWARRAY:
            default:
                pop(iarg);
                pushDesc(sarg);
                break;
        }
        labels = null;
    }
}
