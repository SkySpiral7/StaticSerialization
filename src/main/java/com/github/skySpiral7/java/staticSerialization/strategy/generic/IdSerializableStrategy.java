package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class IdSerializableStrategy implements HeaderStrategy
{
   private static final Logger LOG = LogManager.getLogger();
   private final ObjectReaderRegistry readerRegistry;
   private final IntegerSerializableStrategy integerSerializableStrategy;

   public IdSerializableStrategy(final ObjectReaderRegistry readerRegistry,
                                 final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.readerRegistry = readerRegistry;
      this.integerSerializableStrategy = integerSerializableStrategy;
   }

   @Override
   public boolean supportsHeader(final byte firstByte)
   {
      return '&' == firstByte;
   }

   @Override
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderSerializableStrategy.PartialHeader partialHeader,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      final int id = integerSerializableStrategy.read("Incomplete header: id type but no id");
      final Object registeredObject = readerRegistry.getRegisteredObject(id);
      //null value will not have an id. null is only possible if id was reserved but not registered
      if (registeredObject == null) throw new StreamCorruptedException("id not found");
      //LOG.debug("data.class=" + registeredObject.getClass().getSimpleName() + " val=" + registeredObject + " id=" + id);
      LOG.debug("id: " + id + " (" + registeredObject + " " + registeredObject.getClass().getSimpleName() + ")");
      return HeaderInformation.forValue(partialHeader.firstByte(), registeredObject.getClass().getName(),
         registeredObject);
   }
}
