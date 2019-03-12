package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.Serializable;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

import static com.github.skySpiral7.java.staticSerialization.util.ClassUtil.cast;

public enum AllSerializableStrategy
{
   ;  //no instances

   public static void write(final ObjectStreamWriter streamWriter, final InternalStreamWriter internalStreamWriter,
                            final AsynchronousFileAppender fileAppender, final Object data)
   {
      final Class<?> dataClass = data.getClass();
      if (ClassUtil.isBoxedPrimitive(dataClass))
      {
         BoxPrimitiveSerializableStrategy.write(fileAppender, data);
         return;
      }
      if (data instanceof String)
      {
         StringSerializableStrategy.writeWithLength(fileAppender, (String) data);
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
         EnumSerializableStrategy.write(fileAppender, (Enum<?>) data);
         return;
      }
      if (data instanceof Serializable)
      {
         JavaSerializableStrategy.writeWithLength(fileAppender, (Serializable) data);
         return;
      }

      throw new NotSerializableException(dataClass);
   }

   public static <T> T read(final ObjectStreamReader streamReader, final InternalStreamReader internalStreamReader,
                            final AsynchronousFileReader fileReader, final Class<T> actualClass)
   {
      if (ClassUtil.isBoxedPrimitive(actualClass)) return BoxPrimitiveSerializableStrategy.read(fileReader, actualClass);
      if (String.class.equals(actualClass)) return cast(StringSerializableStrategy.readWithLength(fileReader));
      if (actualClass.isArray())
         return ArraySerializableStrategy.read(streamReader, internalStreamReader, fileReader, actualClass.getComponentType());

      //TODO: has exact same issue: must create empty, register, populate
      if (StaticSerializable.class.isAssignableFrom(actualClass)) return StaticSerializableStrategy.read(streamReader, actualClass);

      if (actualClass.isEnum()) return EnumSerializableStrategy.read(fileReader, actualClass);
      if (Serializable.class.isAssignableFrom(actualClass)) return JavaSerializableStrategy.readWithLength(fileReader);

      throw new NotSerializableException(actualClass);
   }
}
