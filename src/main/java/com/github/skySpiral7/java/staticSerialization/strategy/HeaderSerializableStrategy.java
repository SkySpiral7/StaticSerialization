package com.github.skySpiral7.java.staticSerialization.strategy;

import java.util.HashMap;
import java.util.Map;

import com.github.skySpiral7.java.staticSerialization.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileAppender;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;
import com.github.skySpiral7.java.staticSerialization.util.ArrayUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy.writeByte;

public enum HeaderSerializableStrategy
{
   ;  //no instances
   private static final Logger LOG = LogManager.getLogger();

   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true (header only). and component used for boolean arrays</li>
    * <li>- boolean false (header only. not valid array component)</li>
    * <li>[2 object arrays</li>
    * <li>]2 primitive arrays</li>
    * <li>; null (header only. not valid array component)</li>
    * <li>? inherit type from containing array</li>
    * <li>\id reference existing object (header only)</li>
    * </ul>
    */
   private static final Map<Character, Class<?>> COMPRESSED_HEADER_TO_CLASS;
   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true (header only). and component used for boolean arrays</li>
    * <li>- boolean false (header only. not valid array component)</li>
    * <li>[2 object arrays</li>
    * <li>]2 primitive arrays</li>
    * <li>; null (header only. not valid array component)</li>
    * <li>? inherit type from containing array</li>
    * <li>\id reference existing object (header only)</li>
    * </ul>
    */
   private static final Map<Class<?>, Character> CLASS_TO_COMPRESSED_HEADER;

   static
   {
      COMPRESSED_HEADER_TO_CLASS = new HashMap<>();
      COMPRESSED_HEADER_TO_CLASS.put('~', Byte.class);
      COMPRESSED_HEADER_TO_CLASS.put('!', Short.class);
      COMPRESSED_HEADER_TO_CLASS.put('@', Integer.class);
      COMPRESSED_HEADER_TO_CLASS.put('#', Long.class);
      //$ is allowed to be in class names
      COMPRESSED_HEADER_TO_CLASS.put('%', Float.class);
      COMPRESSED_HEADER_TO_CLASS.put('^', Double.class);
      COMPRESSED_HEADER_TO_CLASS.put('&', Character.class);
      COMPRESSED_HEADER_TO_CLASS.put('*', String.class);

      CLASS_TO_COMPRESSED_HEADER = new HashMap<>();
      CLASS_TO_COMPRESSED_HEADER.put(Byte.class, '~');
      CLASS_TO_COMPRESSED_HEADER.put(Short.class, '!');
      CLASS_TO_COMPRESSED_HEADER.put(Integer.class, '@');
      CLASS_TO_COMPRESSED_HEADER.put(Long.class, '#');
      //$ is allowed to be in class names
      CLASS_TO_COMPRESSED_HEADER.put(Float.class, '%');
      CLASS_TO_COMPRESSED_HEADER.put(Double.class, '^');
      CLASS_TO_COMPRESSED_HEADER.put(Character.class, '&');
      CLASS_TO_COMPRESSED_HEADER.put(String.class, '*');
   }

   /**
    * @param inheritFromClass the component type of the containing array. null if not currently inside an array.
    */
   public static HeaderInformation<?> readHeader(final AsynchronousFileReader reader, final Class<?> inheritFromClass,
                                                 final ObjectReaderRegistry registry)
   {
      byte firstByte;
      final int dimensionCount;
      final boolean primitiveArray;

      //TODO: I don't think it should be excluding Object (even though that's unsupported anyway)
      if (null != inheritFromClass && !Object.class.equals(inheritFromClass))
      {
         //TODO: tests are likely thin
         //inheritFromClass is never primitive void.class.
         //It is only primitive if contained in a primitive array in which case there is no header
         //since it can't be null or any other class.
         if (inheritFromClass.isPrimitive())
            return HeaderInformation.forPrimitiveArrayValue(ClassUtil.boxClass(inheritFromClass).getName());
         //can't ignore header if inheritFromClass is final because it could be null (thus component will be either '?' or ';')
         firstByte = reader.readByte();
         dimensionCount = ArrayUtil.countArrayDimensions(inheritFromClass);
         final Class<?> baseComponent = inheritFromClass.isArray() ? ArrayUtil.getBaseComponentType(inheritFromClass) : inheritFromClass;
         primitiveArray = baseComponent.isPrimitive();
         if ('?' == firstByte)
         {
            return HeaderInformation.forPossibleArray(baseComponent.getName(), dimensionCount, primitiveArray);
         }
         //if inheritFromClass isn't primitive then it is not required to inherit type (eg null or child class) and continues below
      }
      else
      {
         firstByte = reader.readByte();
         if ('?' == firstByte) throw new StreamCorruptedException("Only array elements can inherit type");
         primitiveArray = (']' == firstByte);  //is false if not an array at all
         if ('[' == firstByte || ']' == firstByte)
         {
            if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array dimensions");
            dimensionCount = Byte.toUnsignedInt(reader.readByte());
            if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: no array component type");
            //technically the previous assignment wasn't the first byte but what else can I call this variable?
            firstByte = reader.readByte();
            if (';' == firstByte) throw new StreamCorruptedException("header's array component type can't be null");
            if ('-' == firstByte) throw new StreamCorruptedException("header's array component type can't be false");
            if ('+' == firstByte) return HeaderInformation.forPossibleArray(Boolean.class.getName(), dimensionCount, primitiveArray);
         }
         else dimensionCount = 0;
      }

      if (';' == firstByte) return HeaderInformation.forNull();  //the empty string class name means null
      if ('+' == firstByte) return HeaderInformation.forValue(Boolean.class.getName(), Boolean.TRUE);
      if ('-' == firstByte) return HeaderInformation.forValue(Boolean.class.getName(), Boolean.FALSE);
      if (COMPRESSED_HEADER_TO_CLASS.containsKey((char) firstByte))  //safe cast because map contains only ASCII
      {
         final Class<?> compressedClass = COMPRESSED_HEADER_TO_CLASS.get((char) firstByte);  //safe cast because map contains only ASCII
         return HeaderInformation.forPossibleArray(compressedClass.getName(), dimensionCount, primitiveArray);
      }
      if ('\\' == firstByte)
      {
         //TODO: test
         if (reader.remainingBytes() == 0) throw new StreamCorruptedException("Incomplete header: id type but no id");
         final int id = IntegerSerializableStrategy.read(reader);
         final Object registeredObject = registry.getRegisteredObject(id);
         //null value will not have an id
         if (registeredObject == null) throw new StreamCorruptedException("id not found");
         //LOG.debug("data.class=" + registeredObject.getClass().getSimpleName() + " val=" + registeredObject + " id=" + id);
         LOG.debug("id: " + id + " (" + registeredObject + " " + registeredObject.getClass().getSimpleName() + ")");
         return HeaderInformation.forValue(registeredObject.getClass().getName(), registeredObject);
      }

      //else firstByte is part of a class name
      return HeaderInformation.forPossibleArray(StringSerializableStrategy.readClassName(reader, firstByte), dimensionCount,
            primitiveArray);
   }

   //TODO: rename since true also for null, bool

   /**
    * @return true if an id was used and no value should be written
    */
   public static boolean writeHeaderReturnIsId(final AsynchronousFileAppender appender, final Class<?> inheritFromClass, final Object data,
                                               final ObjectWriterRegistry registry)
   {
      if (data != null && !data.getClass().isPrimitive() && !ClassUtil.isBoxedPrimitive(data.getClass()))
      {
         //TODO: long gets id, other primitives don't, else do
         //TODO: test
         final Integer id = registry.getId(data);
         //LOG.debug("data.class=" + data.getClass().getSimpleName() + " val=" + data + " id=" + id);
         if (id != null)
         {
            LOG.debug("id: " + id + " (" + data + " " + data.getClass().getSimpleName() + ")");
            writeByte(appender, '\\');
            IntegerSerializableStrategy.write(appender, id);
            return true;
         }
         registry.registerObject(data);
      }
      //else if(data==null) LOG.debug("data.class=null");
      //else LOG.debug("data.class=" + data.getClass().getSimpleName() + " val=" + data);

      //boolean[] and Boolean[] use only headers for elements (primitive doesn't allow null)
      if (Boolean.TRUE.equals(data))
      {
         writeByte(appender, '+');
         return true;
      }
      else if (Boolean.FALSE.equals(data))
      {
         writeByte(appender, '-');
         return true;
      }
      else if (data == null)
      {
         //if data is null then class name is the empty string
         writeByte(appender, ';');
         return true;
      }
      //do nothing because non-boolean primitive array elements have no header
      else if (null != inheritFromClass && inheritFromClass.isPrimitive()) ;
         //(below) if class matches containing array exactly then inherit type.
      else if (null != inheritFromClass && inheritFromClass.equals(data.getClass())) writeByte(appender, '?');
      else if (CLASS_TO_COMPRESSED_HEADER.containsKey(data.getClass()))
         writeByte(appender, CLASS_TO_COMPRESSED_HEADER.get(data.getClass()));
      else if (data.getClass().isArray())
      {
         Class<?> baseComponent = ArrayUtil.getBaseComponentType(data.getClass());
         if (Object.class.equals(inheritFromClass) || null == inheritFromClass)
         {
            //array indicator and dimension count can be derived from containing array so don't populate it
            //unless I'm inside Object[] in which case I could have new arrays.
            //baseComponent can't be void or null
            if (baseComponent.isPrimitive())
            {
               writeByte(appender, ']');
               baseComponent = ClassUtil.boxClass(baseComponent);
            }
            else writeByte(appender, '[');

            final int dimensionCount = ArrayUtil.countArrayDimensions(data.getClass());
            writeByte(appender, dimensionCount);  //won't be 0, max: 255. Use unsigned byte
         }

         if (baseComponent.equals(Boolean.class)) writeByte(appender, '+');
         else if (CLASS_TO_COMPRESSED_HEADER.containsKey(baseComponent)) writeByte(appender, CLASS_TO_COMPRESSED_HEADER.get(baseComponent));
         else
         {
            StringSerializableStrategy.writeClassName(appender, baseComponent.getName());
         }
      }
      else
      {
         StringSerializableStrategy.writeClassName(appender, data.getClass().getName());
      }

      return false;
   }
}
