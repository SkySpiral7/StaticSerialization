package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.stream.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum IntegerSerializableStrategy
{
   ;  //no instances
   private static final Logger LOG = LogManager.getLogger();

   public static void write(final AsynchronousFileAppender appender, final int data)
   {
      LOG.debug(data);
      ByteSerializableStrategy.writeBytes(appender, data, 4);
   }

   public static int read(final AsynchronousFileReader reader)
   {
      final int data = BitWiseUtil.bigEndianBytesToInteger(reader.readBytes(4));
      LOG.debug(data);
      return data;
   }
}
