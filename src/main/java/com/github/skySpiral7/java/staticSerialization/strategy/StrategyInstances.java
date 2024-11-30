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
import com.github.skySpiral7.java.staticSerialization.strategy.generic.IdSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.InheritSerializableStrategy;
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
      //null is safe only here
      final NullSerializableStrategy nullSerializableStrategy = new NullSerializableStrategy(null);
      final ShortSerializableStrategy shortSerializableStrategy = new ShortSerializableStrategy(reader);
      final StaticSerializableStrategy staticSerializableStrategy = new StaticSerializableStrategy(streamReader);
      final StringSerializableStrategy stringSerializableStrategy = new StringSerializableStrategy(readerValidationStrategy, reader);

      //ones that need other strategies
      final ArraySerializableStrategy arraySerializableStrategy = new ArraySerializableStrategy(
         readerValidationStrategy,
         reader, streamReader, internalStreamReader, integerSerializableStrategy);
      final ClassHeaderSerializableStrategy classHeaderSerializableStrategy = new ClassHeaderSerializableStrategy(stringSerializableStrategy, readerValidationStrategy);
      final IdSerializableStrategy idSerializableStrategy = new IdSerializableStrategy(registry, integerSerializableStrategy);
      final JavaSerializableStrategy javaSerializableStrategy = new JavaSerializableStrategy(reader, integerSerializableStrategy);
      final EnumSerializableStrategy enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(reader, utilInstances,
         readerValidationStrategy, shortSerializableStrategy, integerSerializableStrategy);
      final BitSetSerializableStrategy bitSetSerializableStrategy =
         new BitSetSerializableStrategy(boxPrimitiveSerializableStrategy, integerSerializableStrategy);
      final UuidSerializableStrategy uuidSerializableStrategy = new UuidSerializableStrategy(boxPrimitiveSerializableStrategy);
      final InheritSerializableStrategy inheritSerializableStrategy = new InheritSerializableStrategy(utilInstances, null);

      this.allSerializableStrategy = new AllSerializableStrategy(reader, utilInstances,
         arraySerializableStrategy,
         bitSetSerializableStrategy,
         boxPrimitiveSerializableStrategy, classHeaderSerializableStrategy, enumSerializableStrategy, idSerializableStrategy,
         inheritSerializableStrategy,
         javaSerializableStrategy,
         nullSerializableStrategy, staticSerializableStrategy, stringSerializableStrategy, uuidSerializableStrategy);
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
      final NullSerializableStrategy nullSerializableStrategy = new NullSerializableStrategy(byteSerializableStrategy);
      final StaticSerializableStrategy staticSerializableStrategy = new StaticSerializableStrategy(streamWriter);

      //ones that need other strategies
      final IntegerSerializableStrategy integerSerializableStrategy = new IntegerSerializableStrategy(byteSerializableStrategy);
      final IdSerializableStrategy idSerializableStrategy = new IdSerializableStrategy(byteSerializableStrategy,
         integerSerializableStrategy, registry);
      final InheritSerializableStrategy inheritSerializableStrategy = new InheritSerializableStrategy(utilInstances,
         byteSerializableStrategy);
      final StringSerializableStrategy stringSerializableStrategy = new StringSerializableStrategy(appender, byteSerializableStrategy);
      final ArraySerializableStrategy arraySerializableStrategy = new ArraySerializableStrategy(utilInstances,
         byteSerializableStrategy, stringSerializableStrategy,
         internalStreamWriter, integerSerializableStrategy);
      final JavaSerializableStrategy javaSerializableStrategy = new JavaSerializableStrategy(appender, byteSerializableStrategy);
      final EnumSerializableStrategy enumSerializableStrategy = new EnumSerializableStrategy(integerSerializableStrategy);
      final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy = new BoxPrimitiveSerializableStrategy(utilInstances,
         byteSerializableStrategy, integerSerializableStrategy);
      final BitSetSerializableStrategy bitSetSerializableStrategy =
         new BitSetSerializableStrategy(boxPrimitiveSerializableStrategy, integerSerializableStrategy);
      final UuidSerializableStrategy uuidSerializableStrategy = new UuidSerializableStrategy(boxPrimitiveSerializableStrategy);
      final ClassHeaderSerializableStrategy classHeaderSerializableStrategy = new ClassHeaderSerializableStrategy(stringSerializableStrategy);

      this.allSerializableStrategy = new AllSerializableStrategy(utilInstances, registry, arraySerializableStrategy,
         bitSetSerializableStrategy,
         boxPrimitiveSerializableStrategy,
         classHeaderSerializableStrategy, enumSerializableStrategy, idSerializableStrategy, inheritSerializableStrategy,
         javaSerializableStrategy,
         nullSerializableStrategy, staticSerializableStrategy, stringSerializableStrategy, uuidSerializableStrategy);
   }

   public AllSerializableStrategy getAllSerializableStrategy()
   {
      return allSerializableStrategy;
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
