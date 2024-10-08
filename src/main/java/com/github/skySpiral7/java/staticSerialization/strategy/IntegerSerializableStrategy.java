package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class IntegerSerializableStrategy
{
   private static final Logger LOG = LogManager.getLogger();

   private final EasyReader reader;
   private final BitWiseUtil bitWiseUtil;
   private final ByteSerializableStrategy byteSerializableStrategy;

   public IntegerSerializableStrategy(final EasyReader reader, final UtilInstances utilInstances)
   {
      this.reader = reader;
      bitWiseUtil = utilInstances.getBitWiseUtil();
      byteSerializableStrategy = null;
   }

   /**
    * For writing
    */
   public IntegerSerializableStrategy(final ByteSerializableStrategy byteSerializableStrategy)
   {
      reader = null;
      bitWiseUtil = null;
      this.byteSerializableStrategy = byteSerializableStrategy;
   }

   public void write(final int data)
   {
      LOG.debug(data);
      byteSerializableStrategy.writeBytes(data, 4);
   }

   public int read(final String corruptMessage)
   {
      //TODO: have everything take a message so that this class can be replaced by BoxPrimitiveSerializableStrategy
      final int data = bitWiseUtil.bigEndianBytesToInteger(
         StreamCorruptedException.throwIfNotEnoughData(reader, 4, corruptMessage)
      );
      LOG.debug(data);
      return data;
   }
}
