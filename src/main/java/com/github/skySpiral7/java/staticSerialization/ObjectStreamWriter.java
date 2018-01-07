package com.github.skySpiral7.java.staticSerialization;

import java.io.Closeable;
import java.io.File;
import java.io.Flushable;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.strategy.AllSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.ReflectionSerializableStrategy;
import com.github.skySpiral7.java.util.FileIoUtil;

public class ObjectStreamWriter implements Closeable, Flushable
{
   private final ObjectWriterRegistry registry = new ObjectWriterRegistry();
   private final AsynchronousFileAppender fileAppender;

   public ObjectStreamWriter(final File destination)
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

   /**
    * List of supported types:
    * <ul>
    * <li>null (not technically a Type.class)</li>
    * <li>Any primitive (except void.class obviously)</li>
    * <li>Any boxed primitive (java.lang.Void.class isn't a box)</li>
    * <li>Any type that extends StaticSerializable</li>
    * <li>Any type that extends Serializable (String and enum have better than normal compression)</li>
    * </ul>
    */
   //for now ignore overloading for all primitives and array stuff
   public void writeObject(final Object data)
   {
      HeaderSerializableStrategy.writeOverhead(fileAppender, data);
      //these cases are only overhead so I'm done
      if (data == null || Boolean.TRUE.equals(data) || Boolean.FALSE.equals(data)) return;
      AllSerializableStrategy.write(this, fileAppender, data);
   }

   public void writeFieldsReflectively(final Object data)
   {
      ReflectionSerializableStrategy.write(this, data);
   }

   public ObjectWriterRegistry getObjectRegistry()
   {
      return registry;
   }
}
