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
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.util.ArrayUtil;
import com.github.skySpiral7.java.util.ClassUtil;
import com.github.skySpiral7.java.util.FileIoUtil;

public class ObjectStreamWriter implements Closeable, Flushable
{
   private final ObjectWriterRegistry registry = new ObjectWriterRegistry();
   private final AsynchronousFileAppender fileAppender;

   /**
    * Not in map:<br/>
    * + boolean true<br/>
    * - boolean false<br/>
    * [2 arrays<br/>
    * ; null<br/>
    */
   private static final Map<Class<?>, Character> COMPRESSED_CLASSES;

   static
   {
      COMPRESSED_CLASSES = new HashMap<>();
      COMPRESSED_CLASSES.put(Byte.class, '~');
      COMPRESSED_CLASSES.put(Short.class, '!');
      COMPRESSED_CLASSES.put(Integer.class, '@');
      COMPRESSED_CLASSES.put(Long.class, '#');
      COMPRESSED_CLASSES.put(Float.class, '%');
      COMPRESSED_CLASSES.put(Double.class, '^');
      COMPRESSED_CLASSES.put(Character.class, '&');
      COMPRESSED_CLASSES.put(String.class, '*');
   }

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
      writeOverhead(data);
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
         final String castedData = (String) data;
         final byte[] writeMe = castedData.getBytes(StandardCharsets.UTF_8);
         writeBytes(writeMe.length, 4);
         fileAppender.append(writeMe);
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
         writePrimitive(castedData.ordinal());
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

   private void writeOverhead(final Object data)
   {
      if (Boolean.TRUE.equals(data)) writeBytes('+', 1);
      else if (Boolean.FALSE.equals(data)) writeBytes('-', 1);
      else if (data == null) writeBytes(';', 1);  //if data is null then class name is the empty string
      else if (COMPRESSED_CLASSES.containsKey(data.getClass())) writeBytes(COMPRESSED_CLASSES.get(data.getClass()), 1);
      else if (data.getClass().isArray())
      {
         writeBytes('[', 1);
         final int dimensionCount = ArrayUtil.countArrayDimensions(data.getClass());
         //TODO: test and remove tests
         if (dimensionCount > 1) throw new UnsupportedOperationException("Currently only 1d arrays are supported");
         writeBytes(dimensionCount, 1);  //won't be 0, max: 255. Use unsigned byte
         final Class<?> baseComponent = ArrayUtil.getBaseComponentType(data.getClass());
         //array type can't be void or null
         //TODO: support primitive arrays
         //         if (baseComponent.equals(boolean.class)) writeBytes('+', 1);
         //         else if (baseComponent.isPrimitive()) writeBytes(COMPRESSED_CLASSES.get(ClassUtil.boxClass(baseComponent)), 1);
         //            //TODO: compress arrays of String and Boxes
         //            //else if (ClassUtil.isBoxedPrimitive(baseComponent))
         //         else
         {
            final byte[] writeMe = baseComponent.getName().getBytes(StandardCharsets.UTF_8);
            fileAppender.append(writeMe);
            writeBytes(';', 1);
         }
         writeBytes(Array.getLength(data), 4);
      }
      else
      {
         final String className = data.getClass().getName();
         //can't use recursion to write the string because that's endless and needs different format
         final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
         fileAppender.append(writeMe);
         writeBytes(';', 1);
         //instead of size then string have the string terminated by ; since this saves 3 bytes and class names can't contain ;
      }
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
