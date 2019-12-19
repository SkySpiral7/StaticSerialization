package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;

import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeBytes;
import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum BoxPrimitiveSerializableStrategy
{
   ;  //no instances

   public static void write(final AsynchronousFileAppender appender, final Object data)
   {
      if (data instanceof Byte) ByteSerializableStrategy.writeByte(appender, (byte) data);
      else if (data instanceof Short) writeBytes(appender, (short) data, 2);
      else if (data instanceof Integer) IntegerSerializableStrategy.write(appender, (int) data);
      else if (data instanceof Long) writeBytes(appender, (long) data, 8);
      else if (data instanceof Float)
      {
         final int castedData = Float.floatToIntBits((float) data);
         //intentionally normalizes NaN
         writeBytes(appender, castedData, 4);
      }
      else if (data instanceof Double)
      {
         long castedData = Double.doubleToLongBits((double) data);
         //intentionally normalizes NaN
         writeBytes(appender, castedData, 8);
      }
      //Boolean won't come here because the value is header only
      //TODO: can a null Boolean[] get here?
      else if (data instanceof Character) writeBytes(appender, (char) data, 2);
      else throw new AssertionError("Method shouldn't've been called");
   }

   public static <T> T read(final AsynchronousFileReader reader, final Class<T> expectedClass)
   {
      if (Byte.class.equals(expectedClass)) return cast(reader.readByte());
      if (Short.class.equals(expectedClass))
      {
         return cast(ShortSerializableStrategy.read(reader));
      }
      if (Integer.class.equals(expectedClass))
      {
         return cast(IntegerSerializableStrategy.read(reader));
      }
      if (Long.class.equals(expectedClass))
      {
         return cast(BitWiseUtil.bigEndianBytesToLong(reader.readBytes(8)));
      }
      if (Float.class.equals(expectedClass))
      {
         final byte[] data = reader.readBytes(4);
         final int intData = BitWiseUtil.bigEndianBytesToInteger(data);
         return cast(Float.intBitsToFloat(intData));
      }
      if (Double.class.equals(expectedClass))
      {
         final byte[] data = reader.readBytes(8);
         final long longData = BitWiseUtil.bigEndianBytesToLong(data);
         return cast(Double.longBitsToDouble(longData));
      }
      if (Boolean.class.equals(expectedClass))
      {
         //TODO: should be reachable through Boolean[] as well. else forbid null here
         //Code is only reachable through primitive boolean arrays, else the header contains the value.
         //Also reachable if a custom written stream uses a header of Boolean.class.
         final byte data = reader.readByte();
         if ('+' == data) return cast(Boolean.TRUE);
         if ('-' == data) return cast(Boolean.FALSE);
         if (';' == data) return null;
         throw new StreamCorruptedException(data + " is not a boolean value");
      }
      if (Character.class.equals(expectedClass))
      {
         return cast((char) ShortSerializableStrategy.read(reader));
      }

      throw new AssertionError("Method shouldn't've been called");
   }
}
