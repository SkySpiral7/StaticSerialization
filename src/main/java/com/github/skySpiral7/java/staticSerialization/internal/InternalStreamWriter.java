package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.StrategyInstances;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;

public class InternalStreamWriter implements Closeable, Flushable
{
   private final EasyAppender appender;
   private final AllSerializableStrategy allSerializableStrategy;
   private final HeaderSerializableStrategy headerSerializableStrategy;
   private final ReflectionSerializableStrategy reflectionSerializableStrategy;

   public InternalStreamWriter(final ObjectStreamWriter streamWriter, final File destination)
   {
      this(streamWriter, new AsynchronousFileAppender(destination));
   }

   public InternalStreamWriter(final ObjectStreamWriter streamWriter, final EasyAppender appender)
   {
      this(streamWriter, appender, new ObjectWriterRegistry(), new UtilInstances());
   }

   public InternalStreamWriter(final ObjectStreamWriter streamWriter, final EasyAppender appender,
                               final ObjectWriterRegistry registry,
                               final UtilInstances utilInstances)
   {
      final StrategyInstances strategyInstances = new StrategyInstances(streamWriter, this, appender, registry,
         utilInstances);
      this.appender = appender;
      allSerializableStrategy = strategyInstances.getAllSerializableStrategy();
      headerSerializableStrategy = strategyInstances.getHeaderSerializableStrategy();
      reflectionSerializableStrategy = strategyInstances.getReflectionSerializableStrategy();
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

   public void writeObjectInternal(final Class<?> inheritFromClass, final Object data)
   {
      final boolean usedId = headerSerializableStrategy.writeHeaderReturnIsId(inheritFromClass, data);
      //if an id was written then don't write value
      if (usedId) return;
      allSerializableStrategy.write(data);
   }

   public ReflectionSerializableStrategy getReflectionSerializableStrategy()
   {
      return reflectionSerializableStrategy;
   }
}
