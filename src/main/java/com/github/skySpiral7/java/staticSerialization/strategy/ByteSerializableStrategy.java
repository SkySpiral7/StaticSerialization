package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;

public enum ByteSerializableStrategy
{
   ;  //no instances

   public static void writeByte(final EasyAppender appender, final int data)
   {
      appender.append((byte) data);
   }

   public static void writeBytes(final EasyAppender appender, long data, final int byteCount)
   {
      appender.append(BitWiseUtil.toBigEndianBytes(data, byteCount));
   }
}
