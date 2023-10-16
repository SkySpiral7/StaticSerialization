package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

public enum ByteSerializableStrategy
{
   ;  //no instances

   public static void writeByte(final EasyAppender appender, final int data)
   {
      appender.append((byte) data);
   }

   public static void writeBytes(final InternalStreamWriter internalStreamWriter, long data, final int byteCount)
   {
      final UtilInstances utilInstances = internalStreamWriter.getUtilInstances();
      final EasyAppender appender = internalStreamWriter.getAppender();
      appender.append(utilInstances.getBitWiseUtil().toBigEndianBytes(data, byteCount));
   }
}
