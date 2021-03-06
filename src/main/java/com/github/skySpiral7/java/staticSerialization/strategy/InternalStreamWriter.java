package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileAppender;
import com.github.skySpiral7.java.util.FileIoUtil;

public class InternalStreamWriter implements Closeable, Flushable
{
   private final AsynchronousFileAppender fileAppender;

   public InternalStreamWriter(final File destination)
   {
      //start by clearing the file so that all writes can append (also this is fail fast to prove that writing is possible)
      FileIoUtil.writeToFile(destination, "");  //must do before fileAppender is created so that the file won't be locked
      fileAppender = new AsynchronousFileAppender(destination);
   }

   /**
    * @see AsynchronousFileAppender#flush()
    */
   @Override
   public void flush(){fileAppender.flush();}

   /**
    * @see AsynchronousFileAppender#close()
    */
   @Override
   public void close(){fileAppender.close();}

   public void writeObjectInternal(final ObjectStreamWriter streamWriter, final Class<?> inheritFromClass, final Object data)
   {
      HeaderSerializableStrategy.writeHeader(fileAppender, inheritFromClass, data);
      //these cases are only header so I'm done
      if (null == data || Boolean.TRUE.equals(data) || Boolean.FALSE.equals(data)) return;
      AllSerializableStrategy.write(streamWriter, this, fileAppender, data);
   }

   public AsynchronousFileAppender getFileAppender()
   {
      return fileAppender;
   }
}
