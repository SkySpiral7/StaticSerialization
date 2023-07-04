package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;

public enum ByteSerializableStrategy
{
   ;  //no instances

   public static void writeByte(final AsynchronousFileAppender appender, final int data)
   {
      appender.append(new byte[]{(byte) data});
   }

   public static void writeBytes(final AsynchronousFileAppender appender, long data, final int byteCount)
   {
      appender.append(BitWiseUtil.toBigEndianBytes(data, byteCount));
   }
}
