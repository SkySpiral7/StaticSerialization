package com.github.skySpiral7.java.staticSerialization.strategy;

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
   public final AllSerializableStrategy allSerializableStrategy;
   public final ArraySerializableStrategy arraySerializableStrategy;
   public final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy;
   public final ByteSerializableStrategy byteSerializableStrategy;
   public final EnumSerializableStrategy enumSerializableStrategy;
   public final HeaderSerializableStrategy headerSerializableStrategy;
   public final IntegerSerializableStrategy integerSerializableStrategy;
   public final JavaSerializableStrategy javaSerializableStrategy;
   public final ReaderValidationStrategy readerValidationStrategy;
   public final ReflectionSerializableStrategy reflectionSerializableStrategy;
   public final ShortSerializableStrategy shortSerializableStrategy;
   public final StaticSerializableStrategy staticSerializableStrategy;
   public final StringSerializableStrategy stringSerializableStrategy;

   public StrategyInstances(final EasyReader reader, final ObjectReaderRegistry registry,
                            final UtilInstances utilInstances)
   {
      //TODO: DI for StrategyInstances
      this.allSerializableStrategy = null;
      this.arraySerializableStrategy = null;
      this.byteSerializableStrategy = null;  //don't need
      this.integerSerializableStrategy = new IntegerSerializableStrategy(reader, utilInstances);
      this.readerValidationStrategy = new ReaderValidationStrategy(utilInstances);
      this.reflectionSerializableStrategy = new ReflectionSerializableStrategy(utilInstances);
      this.shortSerializableStrategy = new ShortSerializableStrategy(reader);
      this.staticSerializableStrategy = null;

      //ones that need other strategies
      this.javaSerializableStrategy = new JavaSerializableStrategy(reader, integerSerializableStrategy);
      this.enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      this.stringSerializableStrategy = new StringSerializableStrategy(reader, integerSerializableStrategy);
      this.boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(reader, utilInstances,
         shortSerializableStrategy, integerSerializableStrategy);
      this.headerSerializableStrategy = new HeaderSerializableStrategy(reader, registry, utilInstances,
         integerSerializableStrategy, stringSerializableStrategy);
   }

   public StrategyInstances(final EasyAppender appender, final ObjectWriterRegistry registry,
                            final UtilInstances utilInstances)
   {
      this.allSerializableStrategy = null;
      this.arraySerializableStrategy = null;
      this.byteSerializableStrategy = new ByteSerializableStrategy(appender, utilInstances);
      this.readerValidationStrategy = null;  //don't need
      this.reflectionSerializableStrategy = new ReflectionSerializableStrategy(utilInstances);
      this.shortSerializableStrategy = null;  //don't need
      this.staticSerializableStrategy = null;

      //ones that need other strategies
      this.integerSerializableStrategy = new IntegerSerializableStrategy(byteSerializableStrategy);
      this.javaSerializableStrategy = new JavaSerializableStrategy(appender, byteSerializableStrategy);
      this.enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      this.stringSerializableStrategy = new StringSerializableStrategy(appender, byteSerializableStrategy,
         integerSerializableStrategy);
      this.boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(utilInstances,
         byteSerializableStrategy, integerSerializableStrategy);
      this.headerSerializableStrategy = new HeaderSerializableStrategy(registry, utilInstances,
         byteSerializableStrategy, integerSerializableStrategy, stringSerializableStrategy);
   }

   public AllSerializableStrategy getAllSerializableStrategy()
   {
      return allSerializableStrategy;
   }

   public ArraySerializableStrategy getArraySerializableStrategy()
   {
      return arraySerializableStrategy;
   }

   public BoxPrimitiveSerializableStrategy getBoxPrimitiveSerializableStrategy()
   {
      return boxPrimitiveSerializableStrategy;
   }

   public ByteSerializableStrategy getByteSerializableStrategy()
   {
      return byteSerializableStrategy;
   }

   public EnumSerializableStrategy getEnumSerializableStrategy()
   {
      return enumSerializableStrategy;
   }

   public HeaderSerializableStrategy getHeaderSerializableStrategy()
   {
      return headerSerializableStrategy;
   }

   public IntegerSerializableStrategy getIntegerSerializableStrategy()
   {
      return integerSerializableStrategy;
   }

   public JavaSerializableStrategy getJavaSerializableStrategy()
   {
      return javaSerializableStrategy;
   }

   public ReaderValidationStrategy getReaderValidationStrategy()
   {
      return readerValidationStrategy;
   }

   public ReflectionSerializableStrategy getReflectionSerializableStrategy()
   {
      return reflectionSerializableStrategy;
   }

   public ShortSerializableStrategy getShortSerializableStrategy()
   {
      return shortSerializableStrategy;
   }

   public StaticSerializableStrategy getStaticSerializableStrategy()
   {
      return staticSerializableStrategy;
   }

   public StringSerializableStrategy getStringSerializableStrategy()
   {
      return stringSerializableStrategy;
   }
}
