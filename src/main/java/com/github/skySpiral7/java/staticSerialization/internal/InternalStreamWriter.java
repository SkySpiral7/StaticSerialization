package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.StrategyInstances;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.HeaderStrategy;
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
   private final ReflectionSerializableStrategy reflectionSerializableStrategy;

   public InternalStreamWriter(final ObjectStreamWriter streamWriter, final File destination)
   {
      this(streamWriter, new AsynchronousFileAppender(destination));
   }

   public InternalStreamWriter(final ObjectStreamWriter streamWriter, final EasyAppender appender)
   {
      final UtilInstances utilInstances=new UtilInstances();
      final ObjectWriterRegistry registry=new ObjectWriterRegistry(utilInstances.getClassUtil());
      final StrategyInstances strategyInstances = new StrategyInstances(streamWriter, this, appender, registry,
         utilInstances);
      this.appender = appender;
      allSerializableStrategy = strategyInstances.getAllSerializableStrategy();
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
      final HeaderStrategy headerStrategy = allSerializableStrategy.determineHeaderStrategy(inheritFromClass, data);
      if (!allSerializableStrategy.writeHeader(headerStrategy, inheritFromClass, data))
         allSerializableStrategy.writeData(data, headerStrategy);
   }

   public ReflectionSerializableStrategy getReflectionSerializableStrategy()
   {
      return reflectionSerializableStrategy;
   }
}
