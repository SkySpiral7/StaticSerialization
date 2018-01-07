package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.util.BitWiseUtil;

import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeBytes;
import static com.github.skySpiral7.java.util.ClassUtil.cast;

public enum PrimitiveSerializableStrategy
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
      else if (data instanceof Boolean)
      {
         if ((boolean) data) ByteSerializableStrategy.writeByte(appender, 1);  //write true as 1
         else ByteSerializableStrategy.writeByte(appender, 0);
      }
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
         //This code is obsolete but still permitted. It isn't normally reached due to the new boolean overhead.
         final byte data = reader.readByte();
         if (data == 1) return cast(Boolean.TRUE);
         return cast(Boolean.FALSE);
      }
      if (Character.class.equals(expectedClass))
      {
         return cast((char) ShortSerializableStrategy.read(reader));
      }

      throw new AssertionError("Method shouldn't've been called");
   }
}
