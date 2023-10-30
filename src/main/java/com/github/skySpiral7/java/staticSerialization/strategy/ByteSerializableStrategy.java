package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

public class ByteSerializableStrategy
{
   private final EasyAppender appender;
   private final BitWiseUtil bitWiseUtil;

   public ByteSerializableStrategy(final EasyAppender appender, final UtilInstances utilInstances)
   {
      this.appender = appender;
      this.bitWiseUtil = utilInstances.getBitWiseUtil();
   }

   public void writeByte(final int data)
   {
      appender.append((byte) data);
   }

   public void writeBytes(long data, final int byteCount)
   {
      appender.append(bitWiseUtil.toBigEndianBytes(data, byteCount));
   }
}
