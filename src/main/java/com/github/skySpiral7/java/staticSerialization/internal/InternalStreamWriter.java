package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
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
   private final ObjectWriterRegistry registry;
   private final UtilInstances utilInstances;
   private final StrategyInstances strategyInstances;

   public InternalStreamWriter(final File destination)
   {
      this(new AsynchronousFileAppender(destination));
   }

   public InternalStreamWriter(final EasyAppender appender)
   {
      this(appender, new ObjectWriterRegistry(), new UtilInstances());
   }

   public InternalStreamWriter(final EasyAppender appender, final ObjectWriterRegistry registry,
                               final UtilInstances utilInstances)
   {
      this(appender, registry, utilInstances, new StrategyInstances(appender, registry, utilInstances));
   }

   public InternalStreamWriter(final EasyAppender appender, final ObjectWriterRegistry registry,
                               final UtilInstances utilInstances, final StrategyInstances strategyInstances)
   {
      this.appender = appender;
      this.registry = registry;
      this.utilInstances = utilInstances;
      this.strategyInstances = strategyInstances;
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
      final boolean usedId = strategyInstances.getHeaderSerializableStrategy().writeHeaderReturnIsId(this,
         inheritFromClass, data);
      //if an id was written then don't write value
      if (usedId) return;
      AllSerializableStrategy.write(streamWriter, this, data);
   }

   public EasyAppender getAppender()
   {
      return appender;
   }

   public ObjectWriterRegistry getRegistry()
   {
      return registry;
   }

   public UtilInstances getUtilInstances()
   {
      return utilInstances;
   }

   public StrategyInstances getStrategyInstances()
   {
      return strategyInstances;
   }
}
