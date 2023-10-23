package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public enum IntegerSerializableStrategy
{
   ;  //no instances
   private static final Logger LOG = LogManager.getLogger();

   public static void write(final InternalStreamWriter internalStreamWriter, final int data)
   {
      LOG.debug(data);
      internalStreamWriter.getStrategyInstances().getByteSerializableStrategy().writeBytes(data, 4);
   }

   public static int read(final InternalStreamReader internalStreamReader, final String corruptMessage)
   {
      final UtilInstances utilInstances = internalStreamReader.getUtilInstances();
      final EasyReader reader = internalStreamReader.getReader();
      final int data = utilInstances.getBitWiseUtil().bigEndianBytesToInteger(
         StreamCorruptedException.throwIfNotEnoughData(reader, 4, corruptMessage)
      );
      LOG.debug(data);
      return data;
   }
}
