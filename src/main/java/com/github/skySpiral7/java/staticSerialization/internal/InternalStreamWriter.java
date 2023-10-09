package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;

public class InternalStreamWriter implements Closeable, Flushable
{
   private final ObjectWriterRegistry registry;
   private final EasyAppender appender;
   private final UtilInstances utilInstances;

   public InternalStreamWriter(final File destination)
   {
      this(new AsynchronousFileAppender(destination));
   }

   public InternalStreamWriter(final EasyAppender appender)
   {
      this(new ObjectWriterRegistry(), appender, new UtilInstances());
   }

   public InternalStreamWriter(ObjectWriterRegistry registry, EasyAppender appender, UtilInstances utilInstances)
   {
      this.registry = registry;
      this.appender = appender;
      this.utilInstances = utilInstances;
   }

   /**
    * @see AsynchronousFileAppender#flush()
    */
   @Override
   public void flush(){appender.flush();}

   /**
    * @see AsynchronousFileAppender#close()
    */
   @Override
   public void close(){appender.close();}

   public void writeObjectInternal(final ObjectStreamWriter streamWriter, final Class<?> inheritFromClass, final Object data)
   {
      final boolean usedId = HeaderSerializableStrategy.writeHeaderReturnIsId(this, inheritFromClass, data);
      //if an id was written then don't write value
      if (usedId) return;
      AllSerializableStrategy.write(streamWriter, this, appender, data);
   }

   public ObjectWriterRegistry getRegistry()
   {
      return registry;
   }

   public EasyAppender getAppender()
   {
      return appender;
   }

   public UtilInstances getUtilInstances()
   {
      return utilInstances;
   }
}
