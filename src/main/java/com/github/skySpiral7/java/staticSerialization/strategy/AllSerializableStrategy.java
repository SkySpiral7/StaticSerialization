package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ArraySerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BitSetSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BoxPrimitiveSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ClassHeaderSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.DataStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.EnumSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.HeaderStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.IdSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.InheritSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.JavaSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.NullSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StaticSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.UuidSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.ArrayUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.util.List;

public class AllSerializableStrategy
{
   private final EasyReader reader;
   private final ArrayUtil arrayUtil;
   private final ClassUtil classUtil;
   private final ObjectWriterRegistry writerRegistry;
   private final List<HeaderStrategy> headerStrategyList;
   private final List<DataStrategy> dataStrategyList;

   /**
    * For reading
    */
   public AllSerializableStrategy(final EasyReader reader,
                                  final UtilInstances utilInstances,
                                  final ArraySerializableStrategy arraySerializableStrategy,
                                  final BitSetSerializableStrategy bitSetSerializableStrategy,
                                  final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                  final ClassHeaderSerializableStrategy classHeaderSerializableStrategy,
                                  final EnumSerializableStrategy enumSerializableStrategy,
                                  final IdSerializableStrategy idSerializableStrategy,
                                  final InheritSerializableStrategy inheritSerializableStrategy,
                                  final JavaSerializableStrategy javaSerializableStrategy,
                                  final NullSerializableStrategy nullSerializableStrategy,
                                  final StaticSerializableStrategy staticSerializableStrategy,
                                  final StringSerializableStrategy stringSerializableStrategy,
                                  final UuidSerializableStrategy uuidSerializableStrategy)
   {
      this(reader,
         utilInstances.getArrayUtil(),
         utilInstances.getClassUtil(),
         null,
         arraySerializableStrategy,
         bitSetSerializableStrategy,
         boxPrimitiveSerializableStrategy, classHeaderSerializableStrategy, enumSerializableStrategy, idSerializableStrategy,
         inheritSerializableStrategy,
         javaSerializableStrategy,
         nullSerializableStrategy, staticSerializableStrategy, stringSerializableStrategy, uuidSerializableStrategy);
   }

   /**
    * For writing
    */
   public AllSerializableStrategy(final UtilInstances utilInstances,
                                  final ObjectWriterRegistry writerRegistry,
                                  final ArraySerializableStrategy arraySerializableStrategy,
                                  final BitSetSerializableStrategy bitSetSerializableStrategy,
                                  final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                  final ClassHeaderSerializableStrategy classHeaderSerializableStrategy,
                                  final EnumSerializableStrategy enumSerializableStrategy,
                                  final IdSerializableStrategy idSerializableStrategy,
                                  final InheritSerializableStrategy inheritSerializableStrategy,
                                  final JavaSerializableStrategy javaSerializableStrategy,
                                  final NullSerializableStrategy nullSerializableStrategy,
                                  final StaticSerializableStrategy staticSerializableStrategy,
                                  final StringSerializableStrategy stringSerializableStrategy,
                                  final UuidSerializableStrategy uuidSerializableStrategy)
   {
      this(null,
         null,
         utilInstances.getClassUtil(),
         writerRegistry,
         arraySerializableStrategy,
         bitSetSerializableStrategy,
         boxPrimitiveSerializableStrategy, classHeaderSerializableStrategy, enumSerializableStrategy, idSerializableStrategy,
         inheritSerializableStrategy,
         javaSerializableStrategy,
         nullSerializableStrategy, staticSerializableStrategy, stringSerializableStrategy, uuidSerializableStrategy);
   }

   /**
    * All args that all constructors call
    */
   public AllSerializableStrategy(final EasyReader reader,
                                  final ArrayUtil arrayUtil,
                                  final ClassUtil classUtil,
                                  final ObjectWriterRegistry writerRegistry,
                                  final ArraySerializableStrategy arraySerializableStrategy,
                                  final BitSetSerializableStrategy bitSetSerializableStrategy,
                                  final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                  final ClassHeaderSerializableStrategy classHeaderSerializableStrategy,
                                  final EnumSerializableStrategy enumSerializableStrategy,
                                  final IdSerializableStrategy idSerializableStrategy,
                                  final InheritSerializableStrategy inheritSerializableStrategy,
                                  final JavaSerializableStrategy javaSerializableStrategy,
                                  final NullSerializableStrategy nullSerializableStrategy,
                                  final StaticSerializableStrategy staticSerializableStrategy,
                                  final StringSerializableStrategy stringSerializableStrategy,
                                  final UuidSerializableStrategy uuidSerializableStrategy)
   {
      this.reader = reader;
      this.arrayUtil = arrayUtil;
      this.classUtil = classUtil;
      this.writerRegistry = writerRegistry;

      /* order:
       * first is supported jdk final classes (none of which are static) so that they have better compression than java.
       * then static so that it will respect any manual serial.
       * then bitset/enum (which can be static) so that the non-static ones will have better compression than java.
       * then java if all else fails */
      dataStrategyList = List.of(
         boxPrimitiveSerializableStrategy, stringSerializableStrategy, arraySerializableStrategy,
         uuidSerializableStrategy,
         staticSerializableStrategy,
         bitSetSerializableStrategy, enumSerializableStrategy,
         javaSerializableStrategy);
      /*
       * TODO: also Big int/dec, stream: new ArrayList<>().stream().collect(Collectors.toList()).toArray()
       * there's already enum. big int/dec could do same
       * only auto do it if it is always more compressed and accounts for all data
       * big int -> byte[]
       * big dec -> toEngineeringString? can't see any way to get base big int
       */

      /* order:
       * null so that the rest don't need to null check.
       * id trumps data and headers.
       * inherit trumps headers but not data.
       * everything else since they don't overlap.
       * lastly class name since that's a catch-all.
       */
      headerStrategyList = List.of(
         nullSerializableStrategy, idSerializableStrategy, inheritSerializableStrategy,
         boxPrimitiveSerializableStrategy, stringSerializableStrategy, arraySerializableStrategy,
         classHeaderSerializableStrategy);
   }

   /**
    * @param inheritFromClass the component type of the containing array. null if not currently inside an array.
    */
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass,
                                          final HeaderInformation.PartialHeader partialHeaderArg,
                                          final Class<?> expectedClass,
                                          final boolean allowChildClass)
   {
      if (null != inheritFromClass && inheritFromClass.isPrimitive())
      {
         //TODO: tests are likely thin
         //inheritFromClass is never primitive void.class.
         //It is only primitive if contained in a primitive array in which case there is no header
         //since it can't be null or any other class.
         return HeaderInformation.forPrimitiveArrayValue(classUtil.boxClass(inheritFromClass));
      }

      final HeaderInformation.PartialHeader partialHeader;
      if (partialHeaderArg != null)
      {
         partialHeader = partialHeaderArg;
      }
      else
      {
         final byte firstByte = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing header")[0];
         //excludes Object for the sake of Object[]
         if (null != inheritFromClass)
            partialHeader = readInheritHeader(inheritFromClass, firstByte);
         else
         {
            if ('?' == firstByte)
               throw new StreamCorruptedException("Only array elements can inherit type");
            partialHeader = new HeaderInformation.PartialHeader(firstByte, 0, false);
         }
      }

      return headerStrategyList.stream()
         .filter(strategy -> strategy.supportsReadingHeader(partialHeader.firstByte()))
         .findFirst()
         .map(strategy -> strategy.readHeader(inheritFromClass, partialHeader, expectedClass, allowChildClass))
         .orElse(null);
   }

   private HeaderInformation.PartialHeader readInheritHeader(final Class<?> inheritFromClass, final byte firstByte)
   {
      if ('?' != firstByte)
      {
         //can't ignore header if inheritFromClass is final because it could be null (thus component will be either '?' or 0xFF)
         final int dimensionCount = arrayUtil.countArrayDimensions(inheritFromClass);
         final Class<?> baseComponent = inheritFromClass.isArray()
            ? arrayUtil.getBaseComponentType(inheritFromClass)
            : inheritFromClass;
         final boolean primitiveArray = baseComponent.isPrimitive();

         //if inheritFromClass isn't primitive then it is not required to inherit type (eg null or child class) and continues below
         return new HeaderInformation.PartialHeader(firstByte, dimensionCount, primitiveArray);
      }
      return new HeaderInformation.PartialHeader(firstByte, 0, false);
   }

   /**
    * @return true if the data was fully represented by a header and thus no more data should be written.
    * false means the header is done but needs data. null means nothing happened (delegate to HeaderSerializableStrategy)
    */
   public boolean writeHeader(final Class<?> inheritFromClass, final Object data)
   {
      final HeaderStrategy headerStrategy = headerStrategyList.stream()
         .filter(strategy -> strategy.supportsWritingHeader(inheritFromClass, data))
         .findFirst()
         .orElseThrow(() -> new AssertionError("Should have used ClassHeaderSerializableStrategy"));

      //TODO: Long should also get id
      if (data != null && writerRegistry.getId(data) == null && !classUtil.isPrimitiveOrBox(data.getClass()))
         //null, primitive, and box don't get registered
         writerRegistry.registerObject(data);

      return headerStrategy.writeHeader(inheritFromClass, data);
   }

   public void writeData(final Object data)
   {
      final Class<?> dataClass = data.getClass();
      dataStrategyList.stream()
         .filter(strategy -> strategy.supportsData(dataClass))
         .findFirst()
         .orElseThrow(() -> new NotSerializableException(dataClass))
         .writeData(data);
   }

   public <T> T readData(final Class<T> actualClass)
   {
      return dataStrategyList.stream()
         .filter(strategy -> strategy.supportsData(actualClass))
         .findFirst()
         .orElseThrow(() -> new NotSerializableException(actualClass))
         .readData(actualClass);
   }
}
