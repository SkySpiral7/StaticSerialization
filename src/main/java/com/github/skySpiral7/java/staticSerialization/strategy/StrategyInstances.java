package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

/**
 * Holds a reference to every strategy object. This is a lazy way to implement DI independent of client DI.
 */
public class StrategyInstances
{
   private final AllSerializableStrategy allSerializableStrategy;
   private final ArraySerializableStrategy arraySerializableStrategy;
   private final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy;
   private final ByteSerializableStrategy byteSerializableStrategy;
   private final EnumSerializableStrategy enumSerializableStrategy;
   private final HeaderSerializableStrategy headerSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;
   private final JavaSerializableStrategy javaSerializableStrategy;
   private final ReaderValidationStrategy readerValidationStrategy;
   private final ReflectionSerializableStrategy reflectionSerializableStrategy;
   private final ShortSerializableStrategy shortSerializableStrategy;
   private final StaticSerializableStrategy staticSerializableStrategy;
   private final StringSerializableStrategy stringSerializableStrategy;

   public StrategyInstances(final ObjectStreamReader streamReader, final EasyReader reader,
                            final ObjectReaderRegistry registry,
                            final UtilInstances utilInstances)
   {
      this.byteSerializableStrategy = null;  //don't need
      this.integerSerializableStrategy = new IntegerSerializableStrategy(reader, utilInstances);
      this.readerValidationStrategy = new ReaderValidationStrategy(utilInstances);
      this.reflectionSerializableStrategy = new ReflectionSerializableStrategy(streamReader, utilInstances);
      this.shortSerializableStrategy = new ShortSerializableStrategy(reader);
      this.staticSerializableStrategy = new StaticSerializableStrategy(streamReader);

      //ones that need other strategies
      this.arraySerializableStrategy = new ArraySerializableStrategy(integerSerializableStrategy);
      this.javaSerializableStrategy = new JavaSerializableStrategy(reader, integerSerializableStrategy);
      this.enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      this.stringSerializableStrategy = new StringSerializableStrategy(reader, integerSerializableStrategy);
      this.boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(reader, utilInstances,
         shortSerializableStrategy, integerSerializableStrategy);
      this.headerSerializableStrategy = new HeaderSerializableStrategy(reader, registry, utilInstances,
         integerSerializableStrategy, stringSerializableStrategy);
      this.allSerializableStrategy = new AllSerializableStrategy(utilInstances, arraySerializableStrategy,
         boxPrimitiveSerializableStrategy, enumSerializableStrategy, javaSerializableStrategy,
         staticSerializableStrategy, stringSerializableStrategy);
   }

   public StrategyInstances(final ObjectStreamWriter streamWriter, final EasyAppender appender,
                            final ObjectWriterRegistry registry,
                            final UtilInstances utilInstances)
   {
      this.byteSerializableStrategy = new ByteSerializableStrategy(appender, utilInstances);
      this.readerValidationStrategy = null;  //don't need
      this.reflectionSerializableStrategy = new ReflectionSerializableStrategy(streamWriter, utilInstances);
      this.shortSerializableStrategy = null;  //don't need
      this.staticSerializableStrategy = new StaticSerializableStrategy(streamWriter);

      //ones that need other strategies
      this.integerSerializableStrategy = new IntegerSerializableStrategy(byteSerializableStrategy);
      this.arraySerializableStrategy = new ArraySerializableStrategy(integerSerializableStrategy);
      this.javaSerializableStrategy = new JavaSerializableStrategy(appender, byteSerializableStrategy);
      this.enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      this.stringSerializableStrategy = new StringSerializableStrategy(appender, byteSerializableStrategy,
         integerSerializableStrategy);
      this.boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(utilInstances,
         byteSerializableStrategy, integerSerializableStrategy);
      this.headerSerializableStrategy = new HeaderSerializableStrategy(registry, utilInstances,
         byteSerializableStrategy, integerSerializableStrategy, stringSerializableStrategy);
      this.allSerializableStrategy = new AllSerializableStrategy(utilInstances, arraySerializableStrategy,
         boxPrimitiveSerializableStrategy, enumSerializableStrategy, javaSerializableStrategy,
         staticSerializableStrategy, stringSerializableStrategy);
   }

   public AllSerializableStrategy getAllSerializableStrategy()
   {
      return allSerializableStrategy;
   }

   public HeaderSerializableStrategy getHeaderSerializableStrategy()
   {
      return headerSerializableStrategy;
   }

   public ReaderValidationStrategy getReaderValidationStrategy()
   {
      return readerValidationStrategy;
   }

   public ReflectionSerializableStrategy getReflectionSerializableStrategy()
   {
      return reflectionSerializableStrategy;
   }
}
