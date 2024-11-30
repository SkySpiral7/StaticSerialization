package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.HeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class IdSerializableStrategy implements HeaderStrategy
{
   private static final Logger LOG = LogManager.getLogger();
   private final ObjectReaderRegistry readerRegistry;
   private final IntegerSerializableStrategy integerSerializableStrategy;
   private final ObjectWriterRegistry writerRegistry;
   private final ClassUtil classUtil;
   private final ByteSerializableStrategy byteSerializableStrategy;

   /**
    * For reading
    */
   public IdSerializableStrategy(final ObjectReaderRegistry readerRegistry,
                                 final IntegerSerializableStrategy integerSerializableStrategy)
   {
      this.readerRegistry = readerRegistry;
      this.integerSerializableStrategy = integerSerializableStrategy;
      this.byteSerializableStrategy = null;
      this.classUtil = null;
      this.writerRegistry = null;
   }

   /**
    * For writing
    */
   public IdSerializableStrategy(final ByteSerializableStrategy byteSerializableStrategy,
                                 final IntegerSerializableStrategy integerSerializableStrategy,
                                 final ClassUtil classUtil,
                                 final ObjectWriterRegistry writerRegistry)
   {
      this.readerRegistry = null;
      this.integerSerializableStrategy = integerSerializableStrategy;
      this.byteSerializableStrategy = byteSerializableStrategy;
      this.classUtil = classUtil;
      this.writerRegistry = writerRegistry;
   }

   @Override
   public boolean supportsReadingHeader(final byte firstByte)
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

   @Override
   public boolean supportsWritingHeader(final Class<?> inheritFromClass, final Object data)
   {
      var result = writerRegistry.getId(data) != null;
      //TODO: Long should also get id
      if (!result && !classUtil.isPrimitiveOrBox(data.getClass()))
         //null, primitive, and box don't get registered
         //TODO: shouldn't have side affect
         writerRegistry.registerObject(data);
      return result;
   }

   @Override
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data)
   {
      final Integer id = writerRegistry.getId(data);
      LOG.debug("id: " + id + " (" + data + " " + data.getClass().getSimpleName() + ")");
      byteSerializableStrategy.writeByte('&');
      integerSerializableStrategy.write(id);
      return true;
   }
}
