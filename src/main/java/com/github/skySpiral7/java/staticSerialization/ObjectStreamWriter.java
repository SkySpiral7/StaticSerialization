package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.Flushable;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.List;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.StringSerializableStrategy;
import com.github.skySpiral7.java.util.ClassUtil;
import com.github.skySpiral7.java.util.FileIoUtil;

import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeByte;
import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeBytes;

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

      final Class<?> dataClass = data.getClass();
      if (ClassUtil.isBoxedPrimitive(dataClass))
      {
         writePrimitive(data);
         return;
      }
      if (data instanceof String)
      {
         StringSerializableStrategy.writeWithLength(fileAppender, (String) data);
         return;
      }
      if (dataClass.isArray())
      {
         //length was written in writeOverhead
         final int length = Array.getLength(data);
         for (int i = 0; i < length; ++i)
         {
            writeObject(Array.get(data, i));
         }
         return;
      }

      //TODO: future: move these into ObjectStreamStrategy. EnumStreamStrategy has read and write.
      if (data instanceof StaticSerializable)
      {
         final StaticSerializable castedData = (StaticSerializable) data;
         castedData.writeToStream(this);
         return;
      }

      if (dataClass.isEnum())
      {
         final Enum<?> castedData = (Enum<?>) data;
         IntegerSerializableStrategy.write(fileAppender, castedData.ordinal());
         return;
      }
      if (data instanceof Serializable)
      {
         final Serializable castedData = (Serializable) data;
         final byte[] serializedData = javaSerialize(castedData);
         writeBytes(fileAppender, serializedData.length, 4);
         fileAppender.append(serializedData);
         return;
      }

      throw new NotSerializableException(dataClass);
   }

   static byte[] javaSerialize(final Serializable castedData)
   {
      final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(512);
      try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
      {
         out.writeObject(castedData);
      }
      catch (final IOException ex)
      {
         throw new StreamCorruptedException(ex);
      }
      return byteStream.toByteArray();
   }

   private void writePrimitive(final Object data)
   {
      if (data instanceof Byte) writeByte(fileAppender, (byte) data);
      else if (data instanceof Short) writeBytes(fileAppender, (short) data, 2);
      else if (data instanceof Integer) writeBytes(fileAppender, (int) data, 4);
      else if (data instanceof Long) writeBytes(fileAppender, (long) data, 8);
      else if (data instanceof Float)
      {
         final int castedData = Float.floatToIntBits((float) data);
         //intentionally normalizes NaN
         writeBytes(fileAppender, castedData, 4);
      }
      else if (data instanceof Double)
      {
         long castedData = Double.doubleToLongBits((double) data);
         //intentionally normalizes NaN
         writeBytes(fileAppender, castedData, 8);
      }
      else if (data instanceof Boolean)
      {
         if ((boolean) data) writeByte(fileAppender, 1);  //write true as 1
         else writeByte(fileAppender, 0);
      }
      else if (data instanceof Character) writeBytes(fileAppender, (char) data, 2);
      else throw new AssertionError("Method shouldn't've been called");
   }

   public void writeFieldsReflectively(final Object data)
   {
      final List<Field> allSerializableFields = SerializationUtil.getAllSerializableFields(data.getClass());
      allSerializableFields.forEach(field -> {
         field.setAccessible(true);
         try
         {
            this.writeObject(field.get(data));
         }
         catch (final IllegalAccessException e)
         {
            throw new AssertionError("This can't be thrown.", e);
            //since I would've gotten SecurityException from setAccessible(true)
         }
      });
   }

   public ObjectWriterRegistry getObjectRegistry()
   {
      return registry;
   }
}
