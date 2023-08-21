package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum IntegerSerializableStrategy
{
   ;  //no instances
   private static final Logger LOG = LogManager.getLogger();

   public static void write(final EasyAppender appender, final int data)
   {
      LOG.debug(data);
      ByteSerializableStrategy.writeBytes(appender, data, 4);
   }

   public static int read(final EasyReader reader)
   {
      //TODO: make sure all readBytes use throwIfNotEnoughData
      return read(reader, "Missing int data");
   }

   public static int read(final EasyReader reader, final String corruptMessage)
   {
      final int data = BitWiseUtil.bigEndianBytesToInteger(
         StreamCorruptedException.throwIfNotEnoughData(reader, 4, corruptMessage)
      );
      LOG.debug(data);
      return data;
   }
}
