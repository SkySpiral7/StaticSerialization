package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum BoxPrimitiveSerializableStrategy
{
   ;  //no instances

   public static void write(final InternalStreamWriter internalStreamWriter, final Object data)
   {
      final ByteSerializableStrategy byteSerializableStrategy =
         internalStreamWriter.getStrategyInstances().getByteSerializableStrategy();
      if (data instanceof Byte) byteSerializableStrategy.writeByte((byte) data);
      else if (data instanceof Short) byteSerializableStrategy.writeBytes((short) data, 2);
      else if (data instanceof Integer) internalStreamWriter.getStrategyInstances().getIntegerSerializableStrategy().write((int) data);
      else if (data instanceof Long) byteSerializableStrategy.writeBytes((long) data, 8);
      else if (data instanceof Float)
      {
         final int castedData = Float.floatToIntBits((float) data);
         //intentionally normalizes NaN
         //TODO: does it need to normalize NaN?
         byteSerializableStrategy.writeBytes(castedData, 4);
      }
      else if (data instanceof Double)
      {
         long castedData = Double.doubleToLongBits((double) data);
         //intentionally normalizes NaN
         byteSerializableStrategy.writeBytes(castedData, 8);
      }
      //Boolean won't come here because the value is header only
      //TODO: can a null Boolean[] get here?
      else if (data instanceof Character) byteSerializableStrategy.writeBytes((char) data, 2);
      else throw new AssertionError("Method shouldn't've been called");
   }

   public static <T> T read(final InternalStreamReader internalStreamReader, final Class<T> expectedClass)
   {
      final UtilInstances utilInstances = internalStreamReader.getUtilInstances();
      final EasyReader reader = internalStreamReader.getReader();
      if (Byte.class.equals(expectedClass))
      {
         return cast(StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing byte data")[0]);
      }
      if (Short.class.equals(expectedClass))
      {
         return cast(ShortSerializableStrategy.read(reader, "Missing short data"));
      }
      if (Integer.class.equals(expectedClass))
      {
         return cast(internalStreamReader.getStrategyInstances().getIntegerSerializableStrategy().read("Missing int data"));
      }
      if (Long.class.equals(expectedClass))
      {
         return cast(utilInstances.getBitWiseUtil().bigEndianBytesToLong(
            StreamCorruptedException.throwIfNotEnoughData(reader, 8, "Missing long data")
         ));
      }
      if (Float.class.equals(expectedClass))
      {
         final byte[] data = StreamCorruptedException.throwIfNotEnoughData(reader, 4, "Missing float data");
         final int intData = utilInstances.getBitWiseUtil().bigEndianBytesToInteger(data);
         return cast(Float.intBitsToFloat(intData));
      }
      if (Double.class.equals(expectedClass))
      {
         final byte[] data = StreamCorruptedException.throwIfNotEnoughData(reader, 8, "Missing double data");
         final long longData = utilInstances.getBitWiseUtil().bigEndianBytesToLong(data);
         return cast(Double.longBitsToDouble(longData));
      }
      if (Boolean.class.equals(expectedClass))
      {
         //TODO: should be reachable through Boolean[] as well. else forbid null here
         //Code is only reachable through primitive boolean arrays, else the header contains the value.
         //Also reachable if a custom written stream uses a header of Boolean.class.
         final byte data = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing boolean data")[0];
         if ('+' == data) return cast(Boolean.TRUE);
         if ('-' == data) return cast(Boolean.FALSE);
         if (';' == data) return null;
         throw new StreamCorruptedException(data + " is not a boolean value");
      }
      if (Character.class.equals(expectedClass))
      {
         return cast((char) ShortSerializableStrategy.read(reader, "Missing char data"));
      }

      throw new AssertionError("Method shouldn't've been called");
   }
}
