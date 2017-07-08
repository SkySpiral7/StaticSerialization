package com.github.SkySpiral7.Java.serialization;

import java.io.*;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.SkySpiral7.Java.AsynchronousFileAppender;
import com.github.SkySpiral7.Java.exception.NotSerializableException;
import com.github.SkySpiral7.Java.exception.StreamCorruptedException;
import com.github.SkySpiral7.Java.util.ClassUtil;
import com.github.SkySpiral7.Java.util.FileIoUtil;

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
   public void flush()
   {
      fileAppender.flush();
   }

   /**
    * @see AsynchronousFileAppender#close()
    */
   @Override
   public void close()
   {
      fileAppender.close();
   }

   private void writeBytes(long data, final int byteCount)
   {
      final byte[] writeMe = new byte[byteCount];
      for (int i = (byteCount - 1); i >= 0; --i)
      {
         //the array is reversed so that it is in big endian
         writeMe[i] = (byte) (data & 0xFF);
         data >>>= 8;
      }
      fileAppender.append(writeMe);
   }

   //for now ignore overloading for all primitives and array stuff
   public void writeObject(final Object data)
   {
      //TODO: for now it doesn't allow arrays
      writeOverhead(data);
      if (data == null || Boolean.TRUE.equals(data) || Boolean.FALSE.equals(data)) return;

      if (ClassUtil.isBoxedPrimitive(data.getClass()))
      {
         writePrimitive(data);
         return;
      }
      if (data.getClass().isAnnotationPresent(GenerateId.class))
      {
         if (registry.getId(data) != null)
         {
            registry.writeId(data, this);
            //if already exists then write the id and stop
            return;
         }
         //else create an id, write it, and continue writing the object
         registry.registerObject(data);
         registry.writeId(data, this);
      }

      if (data instanceof String)
      {
         final String castedData = (String) data;
         final byte[] writeMe = castedData.getBytes(StandardCharsets.UTF_8);
         writeBytes(writeMe.length, 4);
         fileAppender.append(writeMe);
         return;
      }

      if (data instanceof StaticSerializableEnumByName)
      {
         final Enum<?> castedData = (Enum<?>) data;
         writeObject(castedData.name());
         return;
      }
      if (data instanceof StaticSerializableEnumByOrdinal)
      {
         final Enum<?> castedData = (Enum<?>) data;
         writeObject(castedData.ordinal());
         return;
      }

      if (data instanceof StaticSerializable)
      {
         final StaticSerializable castedData = (StaticSerializable) data;
         castedData.writeToStream(this);
         return;
      }
      if (data instanceof Serializable)
      {
         final Serializable castedData = (Serializable) data;
         final byte[] serializedData = javaSerialize(castedData);
         this.writeBytes(serializedData.length, 4);
         fileAppender.append(serializedData);
         return;
      }

      throw new NotSerializableException(data.getClass());
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
      if (data instanceof Byte) writeBytes((byte) data, 1);
      else if (data instanceof Short) writeBytes((short) data, 2);
      else if (data instanceof Integer) writeBytes((int) data, 4);
      else if (data instanceof Long) writeBytes((long) data, 8);
      else if (data instanceof Float)
      {
         final int castedData = Float.floatToIntBits((float) data);
         //intentionally normalizes NaN
         writeBytes(castedData, 4);
      }
      else if (data instanceof Double)
      {
         long castedData = Double.doubleToLongBits((double) data);
         //intentionally normalizes NaN
         writeBytes(castedData, 8);
      }
      else if (data instanceof Boolean)
      {
         if ((boolean) data) writeBytes(1, 1);  //write true
         else writeBytes(0, 1);
      }
      else if (data instanceof Character) writeBytes((char) data, 2);
      else throw new AssertionError("Method shouldn't've been called");
   }

   /**
    * Not in map:<br/>
    + boolean true<br/>
    - boolean false<br/>
    [2<br/>
    | null<br/>
    */
   private static final Map<Class<?>, Character> compressedClasses;
   static {
      compressedClasses = new HashMap<>();
      compressedClasses.put(Byte.class, '~');
      compressedClasses.put(Short.class, '!');
      compressedClasses.put(Integer.class, '@');
      compressedClasses.put(Long.class, '#');
      compressedClasses.put(Float.class, '%');
      compressedClasses.put(Double.class, '^');
      compressedClasses.put(Character.class, '&');
      compressedClasses.put(String.class, '*');
   }
   private void writeOverhead(final Object data)
   {
      if (Boolean.TRUE.equals(data)) writeBytes('+', 1);
      else if (Boolean.FALSE.equals(data)) writeBytes('-', 1);
      else if(data == null) writeBytes('|', 1);  //if data is null then class name is the empty string
      else if (compressedClasses.containsKey(data.getClass())) writeBytes(compressedClasses.get(data.getClass()), 1);
      else
      {
         final String className = data.getClass().getName();
         //can't use recursion to write the string because that's endless and needs different format
         final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
         fileAppender.append(writeMe);
         writeBytes('|', 1);
         //instead of size then string have the string terminated by | since this saves 3 bytes and class names can't contain |
      }
   }

   public void writeFieldsReflectively(final Object data)
   {
      final List<Field> allSerializableFields = SerializationUtil.getAllSerializableFields(data.getClass());
      allSerializableFields.forEach(field ->
      {
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
