package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.ArraySerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.BoxPrimitiveSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.EnumSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.JavaSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StaticSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;

import java.io.Serializable;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public class AllSerializableStrategy
{
   private final ClassUtil classUtil;
   private final ArraySerializableStrategy arraySerializableStrategy;
   private final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy;
   private final EnumSerializableStrategy enumSerializableStrategy;
   private final JavaSerializableStrategy javaSerializableStrategy;
   private final StaticSerializableStrategy staticSerializableStrategy;
   private final StringSerializableStrategy stringSerializableStrategy;

   public AllSerializableStrategy(final UtilInstances utilInstances,
                                  final ArraySerializableStrategy arraySerializableStrategy,
                                  final BoxPrimitiveSerializableStrategy boxPrimitiveSerializableStrategy,
                                  final EnumSerializableStrategy enumSerializableStrategy,
                                  final JavaSerializableStrategy javaSerializableStrategy,
                                  final StaticSerializableStrategy staticSerializableStrategy,
                                  final StringSerializableStrategy stringSerializableStrategy)
   {
      this.classUtil = utilInstances.getClassUtil();
      this.arraySerializableStrategy = arraySerializableStrategy;
      this.boxPrimitiveSerializableStrategy = boxPrimitiveSerializableStrategy;
      this.enumSerializableStrategy = enumSerializableStrategy;
      this.javaSerializableStrategy = javaSerializableStrategy;
      this.staticSerializableStrategy = staticSerializableStrategy;
      this.stringSerializableStrategy = stringSerializableStrategy;
   }

   public void write(final Object data)
   {
      final Class<?> dataClass = data.getClass();
      if (classUtil.isPrimitiveOrBox(dataClass))
      {
         boxPrimitiveSerializableStrategy.write(data);
         return;
      }
      if (data instanceof String)
      {
         stringSerializableStrategy.write(data);
         return;
      }
      if (dataClass.isArray())
      {
         arraySerializableStrategy.write(data);
         return;
      }

      if (data instanceof StaticSerializable)
      {
         staticSerializableStrategy.write(data);
         return;
      }

      if (dataClass.isEnum())
      {
         enumSerializableStrategy.write(data);
         return;
      }
      if (data instanceof Serializable)
      {
         javaSerializableStrategy.write(data);
         return;
      }

      throw new NotSerializableException(dataClass);
   }

   public <T> T read(final Class<T> actualClass)
   {
      if (classUtil.isPrimitiveOrBox(actualClass))
         return boxPrimitiveSerializableStrategy.read(actualClass);
      if (String.class.equals(actualClass))
      {
         return cast(stringSerializableStrategy.read(actualClass));
      }
      if (actualClass.isArray())
         return arraySerializableStrategy.read(actualClass);

      if (StaticSerializable.class.isAssignableFrom(actualClass))
         return staticSerializableStrategy.read(actualClass);

      //TODO: does java serial allow enum data? if yes: JavaSerializableStrategy, if no: doc it
      if (actualClass.isEnum()) return enumSerializableStrategy.read(actualClass);
      if (Serializable.class.isAssignableFrom(actualClass))
         return javaSerializableStrategy.read(actualClass);

      throw new NotSerializableException(actualClass);
   }
}
