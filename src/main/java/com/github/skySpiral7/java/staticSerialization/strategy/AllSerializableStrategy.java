package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.io.Serializable;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum AllSerializableStrategy
{
   ;  //no instances

   public static void write(final ObjectStreamWriter streamWriter, final InternalStreamWriter internalStreamWriter,
                            final Object data)
   {
      final UtilInstances utilInstances = internalStreamWriter.getUtilInstances();
      final StrategyInstances strategyInstances = internalStreamWriter.getStrategyInstances();

      final Class<?> dataClass = data.getClass();
      //TODO: change these to command interface with supports(). compression trick will be first
      if (utilInstances.getClassUtil().isPrimitiveOrBox(dataClass))
      {
         strategyInstances.getBoxPrimitiveSerializableStrategy().write(data);
         return;
      }
      if (data instanceof String)
      {
         strategyInstances.getStringSerializableStrategy().writeWithLength((String) data);
         return;
      }
      if (dataClass.isArray())
      {
         strategyInstances.getArraySerializableStrategy().write(streamWriter, internalStreamWriter, data);
         return;
      }

      if (data instanceof StaticSerializable)
      {
         strategyInstances.getStaticSerializableStrategy().write(streamWriter, (StaticSerializable) data);
         return;
      }

      if (dataClass.isEnum())
      {
         strategyInstances.getEnumSerializableStrategy().write((Enum<?>) data);
         return;
      }
      if (data instanceof Serializable)
      {
         strategyInstances.getJavaSerializableStrategy().writeWithLength((Serializable) data);
         return;
      }

      throw new NotSerializableException(dataClass);
   }

   public static <T> T read(final ObjectStreamReader streamReader, final InternalStreamReader internalStreamReader,
                            final Class<T> actualClass)
   {
      final UtilInstances utilInstances = internalStreamReader.getUtilInstances();
      final StrategyInstances strategyInstances = internalStreamReader.getStrategyInstances();

      if (utilInstances.getClassUtil().isPrimitiveOrBox(actualClass))
         return strategyInstances.getBoxPrimitiveSerializableStrategy().read(actualClass);
      if (String.class.equals(actualClass))
      {
         return cast(strategyInstances.getStringSerializableStrategy().readWithLength());
      }
      if (actualClass.isArray())
         return strategyInstances.getArraySerializableStrategy().read(streamReader, internalStreamReader, actualClass.getComponentType());

      if (StaticSerializable.class.isAssignableFrom(actualClass))
         return strategyInstances.getStaticSerializableStrategy().read(streamReader, actualClass);

      //TODO: does java serial allow enum data? if yes: JavaSerializableStrategy, if no: doc it
      if (actualClass.isEnum()) return strategyInstances.getEnumSerializableStrategy().read(actualClass);
      if (Serializable.class.isAssignableFrom(actualClass))
         return strategyInstances.getJavaSerializableStrategy().readWithLength();

      throw new NotSerializableException(actualClass);
   }
}
