package com.github.skySpiral7.java.staticSerialization;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;

import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;

public class ObjectStreamWriter implements Closeable, Flushable
{
   private final InternalStreamWriter internalStreamWriter;

   public ObjectStreamWriter(final File destination)
   {
      //TODO: allow other streams
      internalStreamWriter = new InternalStreamWriter(destination);
   }

   /**
    * @see AsynchronousFileAppender#flush()
    */
   @Override
   public void flush(){internalStreamWriter.getFileAppender().flush();}

   /**
    * @see AsynchronousFileAppender#close()
    */
   @Override
   public void close(){internalStreamWriter.getFileAppender().close();}

   /**
    * List of supported types:
    * <ul>
    * <li>null (not technically a Type.class)</li>
    * <li>Any primitive (except void.class obviously)</li>
    * <li>Any boxed primitive (java.lang.Void.class isn't a box)</li>
    * <li>String</li>
    * <li>Any type that implements StaticSerializable</li>
    * <li>Any enum</li>
    * <li>Any type that implements Serializable</li>
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
    */
   public void writeObject(final Object data)
   {
      internalStreamWriter.writeObjectInternal(this, null, data);
   }

   public void writeFieldsReflectively(final Object data)
   {
      ReflectionSerializableStrategy.write(this, data);
   }
}
