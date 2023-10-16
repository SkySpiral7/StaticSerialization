package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

import java.io.Serializable;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum AllSerializableStrategy
{
   ;  //no instances

   public static void write(final ObjectStreamWriter streamWriter, final InternalStreamWriter internalStreamWriter,
                            final EasyAppender fileAppender, final Object data)
   {
      final Class<?> dataClass = data.getClass();
      //TODO: change these to command interface with supports(). compression trick will be first
      if (ClassUtil.isPrimitiveOrBox(dataClass))
      {
         BoxPrimitiveSerializableStrategy.write(internalStreamWriter, data);
         return;
      }
      if (data instanceof String)
      {
         StringSerializableStrategy.writeWithLength(internalStreamWriter, (String) data);
         return;
      }
      if (dataClass.isArray())
      {
         ArraySerializableStrategy.write(streamWriter, internalStreamWriter, fileAppender, data);
         return;
      }

      if (data instanceof StaticSerializable)
      {
         StaticSerializableStrategy.write(streamWriter, (StaticSerializable) data);
         return;
      }

      if (dataClass.isEnum())
      {
         EnumSerializableStrategy.write(internalStreamWriter, (Enum<?>) data);
         return;
      }
      if (data instanceof Serializable)
      {
         JavaSerializableStrategy.writeWithLength(internalStreamWriter, (Serializable) data);
         return;
      }

      throw new NotSerializableException(dataClass);
   }

   public static <T> T read(final ObjectStreamReader streamReader, final InternalStreamReader internalStreamReader,
                            final EasyReader fileReader, final Class<T> actualClass)
   {
      if (ClassUtil.isPrimitiveOrBox(actualClass))
         return BoxPrimitiveSerializableStrategy.read(internalStreamReader, actualClass);
      if (String.class.equals(actualClass))
         return cast(StringSerializableStrategy.readWithLength(internalStreamReader));
      if (actualClass.isArray())
         return ArraySerializableStrategy.read(streamReader, internalStreamReader, fileReader, actualClass.getComponentType());

      if (StaticSerializable.class.isAssignableFrom(actualClass))
         return StaticSerializableStrategy.read(streamReader, actualClass);

      //TODO: does java serial allow enum data? yes: JavaSerializableStrategy, no: doc it
      if (actualClass.isEnum()) return EnumSerializableStrategy.read(internalStreamReader, actualClass);
      if (Serializable.class.isAssignableFrom(actualClass))
         return JavaSerializableStrategy.readWithLength(internalStreamReader);

      throw new NotSerializableException(actualClass);
   }
}
