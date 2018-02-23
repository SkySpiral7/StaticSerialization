package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileAppender;

public enum ByteSerializableStrategy
{
   ;  //no instances

   public static void writeByte(final AsynchronousFileAppender appender, final int data)
   {
      appender.append(new byte[]{(byte) data});
   }

   public static void writeBytes(final AsynchronousFileAppender appender, long data, final int byteCount)
   {
      appender.append(ByteSerializableStrategy.toBigEndianBytes(data, byteCount));
   }

   public static byte[] toBigEndianBytes(long data, final int byteCount)
   {
      final byte[] result = new byte[byteCount];
      for (int i = (byteCount - 1); i >= 0; --i)
      {
         //the array is reversed so that it is in big endian
         result[i] = (byte) (data & 0xFF);
         data >>>= 8;
      }
      return result;
   }
}
