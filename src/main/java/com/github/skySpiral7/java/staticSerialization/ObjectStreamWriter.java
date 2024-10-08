package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.io.Serializable;
import java.util.BitSet;

public class ObjectStreamWriter implements Closeable, Flushable
{
   private final InternalStreamWriter internalStreamWriter;

   public ObjectStreamWriter(final File destination)
   {
      internalStreamWriter = new InternalStreamWriter(this, destination);
   }

   public ObjectStreamWriter(final EasyAppender appender)
   {
      internalStreamWriter = new InternalStreamWriter(this, appender);
   }

   /**
    * @see AsynchronousFileAppender#flush()
    */
   @Override
   public void flush(){internalStreamWriter.flush();}

   /**
    * @see AsynchronousFileAppender#close()
    */
   @Override
   public void close(){internalStreamWriter.close();}

   /**
    * List of supported types:
    * <ul>
    * <li>null (not technically a Type.class)</li>
    * <li>Any primitive (except void.class obviously)</li>
    * <li>Any boxed primitive ({@link Void} isn't a box)</li>
    * <li>String</li>
    * <li>Any type that implements {@link StaticSerializable}</li>
    * <li>Any enum</li>
    * <li>Any type that implements {@link Serializable}</li>
    * <li>Primitive Arrays (any number of dimensions)</li>
    * <li>Empty Arrays (any number of dimensions)</li>
    * <li>Arrays (any number of dimensions) which only contain supported elements (the base component need not be supported)</li>
    * </ul>
    *
    * <p>An example of the last item: {@code new Object[]{"Str", 1}} is supported because each of the elements are supported even though
    * Object is not directly supported. But {@code new Object[]{1, new Object()}} is not supported.</p>
    *
    * <p>An Object[] is allowed to contain itself without causing infinite recursion. The self reference is a supported element and the
    * array can be serialized if all other elements are also supported.</p>
    *
    * @implNote Pedantic: A {@link BitSet} written and read will be {@link BitSet#equals(Object)} but will
    * {@link BitSet#trimToSize()} in order to save space. This differs from the behavior of {@link Serializable} and
    * no one should care.</p>
    */
   //TODO: compare all supported types. static write/read and check if java serial changed
   public void writeObject(final Object data)
   {
      internalStreamWriter.writeObjectInternal(null, data);
   }

   public void writeFieldsReflectively(final Object data)
   {
      internalStreamWriter.getReflectionSerializableStrategy().write(data);
   }
}
