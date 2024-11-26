package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ArraySerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BitSetSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BoxPrimitiveSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ClassHeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.EnumSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.JavaSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.NullSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StaticSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.UuidSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

/**
 * Holds a reference to every strategy object. This is a lazy way to implement DI independent of client DI.
 */
public class StrategyInstances
{
   private final AllSerializableStrategy allSerializableStrategy;
   private final HeaderSerializableStrategy headerSerializableStrategy;
   private final ReaderValidationStrategy readerValidationStrategy;
   private final ReflectionSerializableStrategy reflectionSerializableStrategy;

   public StrategyInstances(final ObjectStreamReader streamReader, final InternalStreamReader internalStreamReader,
                            final EasyReader reader,
                            final ObjectReaderRegistry registry,
                            final UtilInstances utilInstances)
   {
      //ones that don't need another strategy
      final IntegerSerializableStrategy integerSerializableStrategy = new IntegerSerializableStrategy(reader, utilInstances);
      this.readerValidationStrategy = new ReaderValidationStrategy(utilInstances);
      this.reflectionSerializableStrategy = new ReflectionSerializableStrategy(streamReader, utilInstances);
      final NullSerializableStrategy nullSerializableStrategy = new NullSerializableStrategy();
      final ShortSerializableStrategy shortSerializableStrategy = new ShortSerializableStrategy(reader);
      final StaticSerializableStrategy staticSerializableStrategy = new StaticSerializableStrategy(streamReader);
      final StringSerializableStrategy stringSerializableStrategy = new StringSerializableStrategy(readerValidationStrategy, reader);

      //ones that need other strategies
      final ArraySerializableStrategy arraySerializableStrategy = new ArraySerializableStrategy(readerValidationStrategy,
         reader, streamReader, internalStreamReader, integerSerializableStrategy);
      final ClassHeaderSerializableStrategy classHeaderSerializableStrategy = new ClassHeaderSerializableStrategy(stringSerializableStrategy, readerValidationStrategy);
      final JavaSerializableStrategy javaSerializableStrategy = new JavaSerializableStrategy(reader, integerSerializableStrategy);
      final EnumSerializableStrategy enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(reader, utilInstances,
         readerValidationStrategy, shortSerializableStrategy, integerSerializableStrategy);
      final BitSetSerializableStrategy bitSetSerializableStrategy =
         new BitSetSerializableStrategy(boxPrimitiveSerializableStrategy, integerSerializableStrategy);
      final UuidSerializableStrategy uuidSerializableStrategy = new UuidSerializableStrategy(boxPrimitiveSerializableStrategy);

      this.allSerializableStrategy = new AllSerializableStrategy(arraySerializableStrategy,
         bitSetSerializableStrategy,
         boxPrimitiveSerializableStrategy, classHeaderSerializableStrategy, enumSerializableStrategy, javaSerializableStrategy,
         nullSerializableStrategy, staticSerializableStrategy, stringSerializableStrategy, uuidSerializableStrategy);
      this.headerSerializableStrategy = new HeaderSerializableStrategy(reader, registry, utilInstances,
         allSerializableStrategy, integerSerializableStrategy, stringSerializableStrategy);
   }

   public StrategyInstances(final ObjectStreamWriter streamWriter, final InternalStreamWriter internalStreamWriter,
                            final EasyAppender appender,
                            final ObjectWriterRegistry registry,
                            final UtilInstances utilInstances)
   {
      final ByteSerializableStrategy byteSerializableStrategy = new ByteSerializableStrategy(appender, utilInstances);
      this.readerValidationStrategy = null;  //don't need
      this.reflectionSerializableStrategy = new ReflectionSerializableStrategy(streamWriter, utilInstances);
      //ones that don't need another strategy
      final NullSerializableStrategy nullSerializableStrategy = new NullSerializableStrategy();
      final StaticSerializableStrategy staticSerializableStrategy = new StaticSerializableStrategy(streamWriter);

      //ones that need other strategies
      final IntegerSerializableStrategy integerSerializableStrategy = new IntegerSerializableStrategy(byteSerializableStrategy);
      final ArraySerializableStrategy arraySerializableStrategy = new ArraySerializableStrategy(internalStreamWriter, integerSerializableStrategy);
      final JavaSerializableStrategy javaSerializableStrategy = new JavaSerializableStrategy(appender, byteSerializableStrategy);
      final EnumSerializableStrategy enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      final StringSerializableStrategy stringSerializableStrategy = new StringSerializableStrategy(appender, byteSerializableStrategy);
      //null arg is only safe because this strat won't be used during write
      final ClassHeaderSerializableStrategy classHeaderSerializableStrategy = new ClassHeaderSerializableStrategy(stringSerializableStrategy, null);
      final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(utilInstances,
         byteSerializableStrategy, integerSerializableStrategy);
      final BitSetSerializableStrategy bitSetSerializableStrategy =
         new BitSetSerializableStrategy(boxPrimitiveSerializableStrategy, integerSerializableStrategy);
      final UuidSerializableStrategy uuidSerializableStrategy = new UuidSerializableStrategy(boxPrimitiveSerializableStrategy);

      this.headerSerializableStrategy = new HeaderSerializableStrategy(registry, utilInstances,
         byteSerializableStrategy, integerSerializableStrategy, stringSerializableStrategy);
      this.allSerializableStrategy = new AllSerializableStrategy(arraySerializableStrategy,
         bitSetSerializableStrategy,
         boxPrimitiveSerializableStrategy, classHeaderSerializableStrategy, enumSerializableStrategy, javaSerializableStrategy,
         nullSerializableStrategy, staticSerializableStrategy, stringSerializableStrategy, uuidSerializableStrategy);
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
