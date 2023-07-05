package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.util.FileIoUtil;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;

public class InternalStreamWriter implements Closeable, Flushable
{
   private final ObjectWriterRegistry registry;
   private final EasyAppender appender;

   public InternalStreamWriter(final File destination)
   {
      registry = new ObjectWriterRegistry();
      //start by clearing the file so that all writes can append (also this is fail fast to prove that writing is possible)
      FileIoUtil.writeToFile(destination, "");  //must do before fileAppender is created so that the file won't be locked
      appender = new AsynchronousFileAppender(destination);
   }

   public InternalStreamWriter(final EasyAppender appender)
   {
      registry = new ObjectWriterRegistry();
      this.appender = appender;
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
      final boolean usedId = HeaderSerializableStrategy.writeHeaderReturnIsId(appender, inheritFromClass, data, registry);
      //if an id was written then don't write value
      if (usedId) return;
      AllSerializableStrategy.write(streamWriter, this, appender, data);
   }

   public EasyAppender getAppender()
   {
      return appender;
   }
}
