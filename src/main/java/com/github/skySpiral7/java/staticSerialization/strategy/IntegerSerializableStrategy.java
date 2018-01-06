package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.AsynchronousFileAppender;
import com.github.skySpiral7.java.AsynchronousFileReader;
import com.github.skySpiral7.java.util.BitWiseUtil;

public enum IntegerSerializableStrategy
{
   ;  //no instances

   public static void write(final AsynchronousFileAppender appender, final int data)
   {
      ByteSerializableStrategy.writeBytes(appender, data, 4);
   }

   public static int read(final AsynchronousFileReader reader)
   {
      return BitWiseUtil.bigEndianBytesToInteger(reader.readBytes(4));
   }
}
