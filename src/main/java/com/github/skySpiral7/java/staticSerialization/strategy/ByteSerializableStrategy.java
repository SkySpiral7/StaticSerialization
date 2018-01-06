package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.AsynchronousFileAppender;

public enum ByteSerializableStrategy
{
   ;  //no instances

   public static void writeByte(final AsynchronousFileAppender appender, final int data)
   {
      appender.append(new byte[]{(byte) data});
   }

   public static void writeBytes(final AsynchronousFileAppender appender, long data, final int byteCount)
   {
      final byte[] writeMe = new byte[byteCount];
      for (int i = (byteCount - 1); i >= 0; --i)
      {
         //the array is reversed so that it is in big endian
         writeMe[i] = (byte) (data & 0xFF);
         data >>>= 8;
      }
      appender.append(writeMe);
   }
}
