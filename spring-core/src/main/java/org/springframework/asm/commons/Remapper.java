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

import org.springframework.asm.signature.SignatureWriter;

/**
 * A class responsible for remapping types and names.
 * Subclasses can override the following methods:
 *
 * <ul>
 * <li>{@link #map(String)} - map type</li>
 * <li>{@link #mapFieldName(String, String, String)} - map field name</li>
 * <li>{@link #mapMethodName(String, String, String)} - map method name</li>
 * </ul>
 *
 * @author Eugene Kuleshov
 */
public abstract class Remapper {

    public String mapDesc(String desc) {
        org.springframework.asm.Type t = org.springframework.asm.Type.getType(desc);
        switch (t.getSort()) {
            case org.springframework.asm.Type.ARRAY:
                String s = mapDesc(t.getElementType().getDescriptor());
                for (int i = 0; i < t.getDimensions(); ++i) {
                    s = '[' + s;
                }
                return s;
            case org.springframework.asm.Type.OBJECT:
                String newType = map(t.getInternalName());
                if (newType != null) {
                    return 'L' + newType + ';';
                }
        }
        return desc;
    }

    private org.springframework.asm.Type mapType(org.springframework.asm.Type t) {
        switch (t.getSort()) {
            case org.springframework.asm.Type.ARRAY:
                String s = mapDesc(t.getElementType().getDescriptor());
                for (int i = 0; i < t.getDimensions(); ++i) {
                    s = '[' + s;
                }
                return org.springframework.asm.Type.getType(s);
            case org.springframework.asm.Type.OBJECT:
                s = map(t.getInternalName());
                return s != null ? org.springframework.asm.Type.getObjectType(s) : t;
            case org.springframework.asm.Type.METHOD:
                return org.springframework.asm.Type.getMethodType(mapMethodDesc(t.getDescriptor()));
        }
        return t;
    }

    public String mapType(String type) {
        if (type == null) {
            return null;
        }
        return mapType(org.springframework.asm.Type.getObjectType(type)).getInternalName();
    }

    public String[] mapTypes(String[] types) {
        String[] newTypes = null;
        boolean needMapping = false;
        for (int i = 0; i < types.length; i++) {
            String type = types[i];
            String newType = map(type);
            if (newType != null && newTypes == null) {
                newTypes = new String[types.length];
                if (i > 0) {
                    System.arraycopy(types, 0, newTypes, 0, i);
                }
                needMapping = true;
            }
            if (needMapping) {
                newTypes[i] = newType == null
                    ? type
                    : newType;
            }
        }
        return needMapping
           ? newTypes
           : types;
    }

    public String mapMethodDesc(String desc) {
        if("()V".equals(desc)) {
            return desc;
        }

        org.springframework.asm.Type[] args = org.springframework.asm.Type.getArgumentTypes(desc);
        StringBuffer s = new StringBuffer("(");
        for (int i = 0; i < args.length; i++) {
            s.append(mapDesc(args[i].getDescriptor()));
        }
        org.springframework.asm.Type returnType = org.springframework.asm.Type.getReturnType(desc);
        if(returnType == org.springframework.asm.Type.VOID_TYPE) {
            s.append(")V");
            return s.toString();
        }
        s.append(')').append(mapDesc(returnType.getDescriptor()));
        return s.toString();
    }

    public Object mapValue(Object value) {
        if (value instanceof org.springframework.asm.Type) {
            return mapType((org.springframework.asm.Type) value);
        }
        if (value instanceof org.springframework.asm.Handle) {
            org.springframework.asm.Handle h = (org.springframework.asm.Handle) value;
            return new org.springframework.asm.Handle(h.getTag(),
                    mapType(h.getOwner()),
                    mapMethodName(h.getOwner(), h.getName(), h.getDesc()),
                    mapMethodDesc(h.getDesc()));
        }
        return value;
    }

    /**
     *
     * @param typeSignature true if signature is a FieldTypeSignature, such as
     *        the signature parameter of the ClassVisitor.visitField or
     *        MethodVisitor.visitLocalVariable methods
     */
    public String mapSignature(String signature, boolean typeSignature) {
        if (signature == null) {
            return null;
        }
        org.springframework.asm.signature.SignatureReader r = new org.springframework.asm.signature.SignatureReader(signature);
        SignatureWriter w = new SignatureWriter();
        org.springframework.asm.signature.SignatureVisitor a = createRemappingSignatureAdapter(w);
        if (typeSignature) {
            r.acceptType(a);
        } else {
            r.accept(a);
        }
        return w.toString();
    }

    protected org.springframework.asm.signature.SignatureVisitor createRemappingSignatureAdapter(
        org.springframework.asm.signature.SignatureVisitor v)
    {
        return new RemappingSignatureAdapter(v, this);
    }

    /**
     * Map method name to the new name. Subclasses can override.
     *
     * @param owner owner of the method.
     * @param name name of the method.
     * @param desc descriptor of the method.
     * @return new name of the method
     */
    public String mapMethodName(String owner, String name, String desc) {
        return name;
    }

    /**
     * Map invokedynamic method name to the new name. Subclasses can override.
     *
     * @param name name of the invokedynamic.
     * @param desc descriptor of the invokedynamic.
     * @return new invokdynamic name.
     */
    public String mapInvokeDynamicMethodName(String name, String desc) {
        return name;
    }

    /**
     * Map field name to the new name. Subclasses can override.
     *
     * @param owner owner of the field.
     * @param name name of the field
     * @param desc descriptor of the field
     * @return new name of the field.
     */
    public String mapFieldName(String owner, String name, String desc) {
        return name;
    }

    /**
     * Map type name to the new name. Subclasses can override.
     *
     */
    public String map(String typeName) {
        return typeName;
    }
}
