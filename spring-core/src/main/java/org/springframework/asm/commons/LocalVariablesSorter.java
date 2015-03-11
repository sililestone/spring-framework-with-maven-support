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

/**
 * A {@link org.springframework.asm.MethodVisitor} that renumbers local variables in their order of
 * appearance. This adapter allows one to easily add new local variables to a
 * method. It may be used by inheriting from this class, but the preferred way
 * of using it is via delegation: the next visitor in the chain can indeed add
 * new locals when needed by calling {@link #newLocal} on this adapter (this
 * requires a reference back to this {@link LocalVariablesSorter}).
 *
 * @author Chris Nokleberg
 * @author Eugene Kuleshov
 * @author Eric Bruneton
 */
public class LocalVariablesSorter extends org.springframework.asm.MethodVisitor {

    private static final org.springframework.asm.Type OBJECT_TYPE = org.springframework.asm.Type.getObjectType("java/lang/Object");

    /**
     * Mapping from old to new local variable indexes. A local variable at index
     * i of size 1 is remapped to 'mapping[2*i]', while a local variable at
     * index i of size 2 is remapped to 'mapping[2*i+1]'.
     */
    private int[] mapping = new int[40];

    /**
     * Array used to store stack map local variable types after remapping.
     */
    private Object[] newLocals = new Object[20];

    /**
     * Index of the first local variable, after formal parameters.
     */
    protected final int firstLocal;

    /**
     * Index of the next local variable to be created by {@link #newLocal}.
     */
    protected int nextLocal;

    /**
     * Indicates if at least one local variable has moved due to remapping.
     */
    private boolean changed;

    /**
     * Creates a new {@link LocalVariablesSorter}. <i>Subclasses must not use
     * this constructor</i>. Instead, they must use the
     * {@link #LocalVariablesSorter(int, int, String, org.springframework.asm.MethodVisitor)} version.
     *
     * @param access access flags of the adapted method.
     * @param desc the method's descriptor (see {@link org.springframework.asm.Type Type}).
     * @param mv the method visitor to which this adapter delegates calls.
     */
    public LocalVariablesSorter(
        final int access,
        final String desc,
        final org.springframework.asm.MethodVisitor mv)
    {
        this(org.springframework.asm.Opcodes.ASM4, access, desc, mv);
    }

    /**
     * Creates a new {@link LocalVariablesSorter}.
     *
     * @param api the ASM API version implemented by this visitor. Must be one
     *        of {@link org.springframework.asm.Opcodes#ASM4}.
     * @param access access flags of the adapted method.
     * @param desc the method's descriptor (see {@link org.springframework.asm.Type Type}).
     * @param mv the method visitor to which this adapter delegates calls.
     */
    protected LocalVariablesSorter(
        final int api,
        final int access,
        final String desc,
        final org.springframework.asm.MethodVisitor mv)
    {
        super(api, mv);
        org.springframework.asm.Type[] args = org.springframework.asm.Type.getArgumentTypes(desc);
        nextLocal = (org.springframework.asm.Opcodes.ACC_STATIC & access) == 0 ? 1 : 0;
        for (int i = 0; i < args.length; i++) {
            nextLocal += args[i].getSize();
        }
        firstLocal = nextLocal;
    }

    @Override
    public void visitVarInsn(final int opcode, final int var) {
        org.springframework.asm.Type type;
        switch (opcode) {
            case org.springframework.asm.Opcodes.LLOAD:
            case org.springframework.asm.Opcodes.LSTORE:
                type = org.springframework.asm.Type.LONG_TYPE;
                break;

            case org.springframework.asm.Opcodes.DLOAD:
            case org.springframework.asm.Opcodes.DSTORE:
                type = org.springframework.asm.Type.DOUBLE_TYPE;
                break;

            case org.springframework.asm.Opcodes.FLOAD:
            case org.springframework.asm.Opcodes.FSTORE:
                type = org.springframework.asm.Type.FLOAT_TYPE;
                break;

            case org.springframework.asm.Opcodes.ILOAD:
            case org.springframework.asm.Opcodes.ISTORE:
                type = org.springframework.asm.Type.INT_TYPE;
                break;

            default:
            // case Opcodes.ALOAD:
            // case Opcodes.ASTORE:
            // case RET:
                type = OBJECT_TYPE;
                break;
        }
        mv.visitVarInsn(opcode, remap(var, type));
    }

    @Override
    public void visitIincInsn(final int var, final int increment) {
        mv.visitIincInsn(remap(var, org.springframework.asm.Type.INT_TYPE), increment);
    }

    @Override
    public void visitMaxs(final int maxStack, final int maxLocals) {
        mv.visitMaxs(maxStack, nextLocal);
    }

    @Override
    public void visitLocalVariable(
        final String name,
        final String desc,
        final String signature,
        final org.springframework.asm.Label start,
        final org.springframework.asm.Label end,
        final int index)
    {
        int newIndex = remap(index, org.springframework.asm.Type.getType(desc));
        mv.visitLocalVariable(name, desc, signature, start, end, newIndex);
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

        if (!changed) { // optimization for the case where mapping = identity
            mv.visitFrame(type, nLocal, local, nStack, stack);
            return;
        }

        // creates a copy of newLocals
        Object[] oldLocals = new Object[newLocals.length];
        System.arraycopy(newLocals, 0, oldLocals, 0, oldLocals.length);

        // copies types from 'local' to 'newLocals'
        // 'newLocals' already contains the variables added with 'newLocal'

        int index = 0; // old local variable index
        int number = 0; // old local variable number
        for (; number < nLocal; ++number) {
            Object t = local[number];
            int size = t == org.springframework.asm.Opcodes.LONG || t == org.springframework.asm.Opcodes.DOUBLE ? 2 : 1;
            if (t != org.springframework.asm.Opcodes.TOP) {
                org.springframework.asm.Type typ = OBJECT_TYPE;
                if (t == org.springframework.asm.Opcodes.INTEGER) {
                    typ = org.springframework.asm.Type.INT_TYPE;
                } else if (t == org.springframework.asm.Opcodes.FLOAT) {
                    typ = org.springframework.asm.Type.FLOAT_TYPE;
                } else if (t == org.springframework.asm.Opcodes.LONG) {
                    typ = org.springframework.asm.Type.LONG_TYPE;
                } else if (t == org.springframework.asm.Opcodes.DOUBLE) {
                    typ = org.springframework.asm.Type.DOUBLE_TYPE;
                } else if (t instanceof String) {
                    typ = org.springframework.asm.Type.getObjectType((String) t);
                }
                setFrameLocal(remap(index, typ), t);
            }
            index += size;
        }

        // removes TOP after long and double types as well as trailing TOPs

        index = 0;
        number = 0;
        for (int i = 0; index < newLocals.length; ++i) {
            Object t = newLocals[index++];
            if (t != null && t != org.springframework.asm.Opcodes.TOP) {
                newLocals[i] = t;
                number = i + 1;
                if (t == org.springframework.asm.Opcodes.LONG || t == org.springframework.asm.Opcodes.DOUBLE) {
                    index += 1;
                }
            } else {
                newLocals[i] = org.springframework.asm.Opcodes.TOP;
            }
        }

        // visits remapped frame
        mv.visitFrame(type, number, newLocals, nStack, stack);

        // restores original value of 'newLocals'
        newLocals = oldLocals;
    }

    // -------------

    /**
     * Creates a new local variable of the given type.
     *
     * @param type the type of the local variable to be created.
     * @return the identifier of the newly created local variable.
     */
    public int newLocal(final org.springframework.asm.Type type) {
        Object t;
        switch (type.getSort()) {
            case org.springframework.asm.Type.BOOLEAN:
            case org.springframework.asm.Type.CHAR:
            case org.springframework.asm.Type.BYTE:
            case org.springframework.asm.Type.SHORT:
            case org.springframework.asm.Type.INT:
                t = org.springframework.asm.Opcodes.INTEGER;
                break;
            case org.springframework.asm.Type.FLOAT:
                t = org.springframework.asm.Opcodes.FLOAT;
                break;
            case org.springframework.asm.Type.LONG:
                t = org.springframework.asm.Opcodes.LONG;
                break;
            case org.springframework.asm.Type.DOUBLE:
                t = org.springframework.asm.Opcodes.DOUBLE;
                break;
            case org.springframework.asm.Type.ARRAY:
                t = type.getDescriptor();
                break;
            // case Type.OBJECT:
            default:
                t = type.getInternalName();
                break;
        }
        int local = nextLocal;
        nextLocal += type.getSize();
        setLocalType(local, type);
        setFrameLocal(local, t);
        return local;
    }

    /**
     * Sets the current type of the given local variable. The default
     * implementation of this method does nothing.
     *
     * @param local a local variable identifier, as returned by {@link #newLocal
     *        newLocal()}.
     * @param type the type of the value being stored in the local variable
     */
    protected void setLocalType(final int local, final org.springframework.asm.Type type) {
    }

    private void setFrameLocal(final int local, final Object type) {
        int l = newLocals.length;
        if (local >= l) {
            Object[] a = new Object[Math.max(2 * l, local + 1)];
            System.arraycopy(newLocals, 0, a, 0, l);
            newLocals = a;
        }
        newLocals[local] = type;
    }

    private int remap(final int var, final org.springframework.asm.Type type) {
        if (var + type.getSize() <= firstLocal) {
            return var;
        }
        int key = 2 * var + type.getSize() - 1;
        int size = mapping.length;
        if (key >= size) {
            int[] newMapping = new int[Math.max(2 * size, key + 1)];
            System.arraycopy(mapping, 0, newMapping, 0, size);
            mapping = newMapping;
        }
        int value = mapping[key];
        if (value == 0) {
            value = newLocalMapping(type);
            setLocalType(value, type);
            mapping[key] = value + 1;
        } else {
            value--;
        }
        if (value != var) {
            changed = true;
        }
        return value;
    }

    protected int newLocalMapping(final org.springframework.asm.Type type) {
        int local = nextLocal;
        nextLocal += type.getSize();
        return local;
    }
}
