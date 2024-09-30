package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.ArrayUtil;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.HashMap;
import java.util.Map;

public class HeaderSerializableStrategy
{
   private static final Logger LOG = LogManager.getLogger();

   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true (header only). and component used for boolean arrays</li>
    * <li>- boolean false (header only. not valid array component)</li>
    * <li>[2Type object arrays</li>
    * <li>]2Type primitive arrays</li>
    * <li>Type normal class which is 0xFF terminated</li>
    * <li>0xFF null (header only. not valid array component)</li>
    * <li>? inherit type from containing array</li>
    * <li>&id reference existing object (header only)</li>
    * </ul>
    */
   private final Map<Character, Class<?>> COMPRESSED_HEADER_TO_CLASS;
   /**
    * Not in map:
    * <ul>
    * <li>+ boolean true (header only). and component used for boolean arrays</li>
    * <li>- boolean false (header only. not valid array component)</li>
    * <li>[2Type object arrays</li>
    * <li>]2Type primitive arrays</li>
    * <li>Type normal class which is 0xFF terminated</li>
    * <li>0xFF null (header only. not valid array component)</li>
    * <li>? inherit type from containing array</li>
    * <li>&id reference existing object (header only)</li>
    * </ul>
    */
   private final Map<Class<?>, Character> CLASS_TO_COMPRESSED_HEADER;

   {
      /*
      possible printable ASCII headers: space to / (not $ or .) is 14, : to @ is +7, [ to ` (not _) is +5, { to ~ is +4 = 30
      I've used 14 so far which leaves 16 free spots
      forbidden: $.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz
      allowed (30): !"#%&'()*+,-/:;<=>?@[\]^`{|}~ space
      used (14): !"#%&'+-?@[]^~
      available (16): ()*,/:;<=>\`{|} space
      technically a FQ class name can't start with a number or dot so I could use them but I won't
      variables names can start with $ so I assume a package/class can too
      */
      COMPRESSED_HEADER_TO_CLASS = new HashMap<>();
      COMPRESSED_HEADER_TO_CLASS.put('~', Byte.class);
      COMPRESSED_HEADER_TO_CLASS.put('!', Short.class);
      COMPRESSED_HEADER_TO_CLASS.put('@', Integer.class);
      COMPRESSED_HEADER_TO_CLASS.put('#', Long.class);
      //$ is allowed to be in class names
      COMPRESSED_HEADER_TO_CLASS.put('%', Float.class);
      COMPRESSED_HEADER_TO_CLASS.put('^', Double.class);
      COMPRESSED_HEADER_TO_CLASS.put('\'', Character.class);
      COMPRESSED_HEADER_TO_CLASS.put('"', String.class);

      CLASS_TO_COMPRESSED_HEADER = new HashMap<>();
      CLASS_TO_COMPRESSED_HEADER.put(Byte.class, '~');
      CLASS_TO_COMPRESSED_HEADER.put(Short.class, '!');
      CLASS_TO_COMPRESSED_HEADER.put(Integer.class, '@');
      CLASS_TO_COMPRESSED_HEADER.put(Long.class, '#');
      //$ is allowed to be in class names
      CLASS_TO_COMPRESSED_HEADER.put(Float.class, '%');
      CLASS_TO_COMPRESSED_HEADER.put(Double.class, '^');
      CLASS_TO_COMPRESSED_HEADER.put(Character.class, '\'');
      CLASS_TO_COMPRESSED_HEADER.put(String.class, '"');
   }

   private final EasyReader reader;
   private final ObjectReaderRegistry readerRegistry;
   private final ObjectWriterRegistry writerRegistry;
   private final ArrayUtil arrayUtil;
   private final ClassUtil classUtil;
   private final ByteSerializableStrategy byteSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;
   private final StringSerializableStrategy stringSerializableStrategy;

   public HeaderSerializableStrategy(final EasyReader reader, final ObjectReaderRegistry registry,
                                     final UtilInstances utilInstances,
                                     final IntegerSerializableStrategy integerSerializableStrategy,
                                     final StringSerializableStrategy stringSerializableStrategy)
   {
      this.reader = reader;
      readerRegistry = registry;
      writerRegistry = null;
      arrayUtil = utilInstances.getArrayUtil();
      classUtil = utilInstances.getClassUtil();
      byteSerializableStrategy = null;
      this.integerSerializableStrategy = integerSerializableStrategy;
      this.stringSerializableStrategy = stringSerializableStrategy;
   }

   public HeaderSerializableStrategy(final ObjectWriterRegistry registry,
                                     final UtilInstances utilInstances, final ByteSerializableStrategy byteSerializableStrategy,
                                     final IntegerSerializableStrategy integerSerializableStrategy,
                                     final StringSerializableStrategy stringSerializableStrategy)
   {
      reader = null;
      readerRegistry = null;
      writerRegistry = registry;
      arrayUtil = utilInstances.getArrayUtil();
      classUtil = utilInstances.getClassUtil();
      this.byteSerializableStrategy = byteSerializableStrategy;
      this.integerSerializableStrategy = integerSerializableStrategy;
      this.stringSerializableStrategy = stringSerializableStrategy;
   }

   /**
    * @param inheritFromClass the component type of the containing array. null if not currently inside an array.
    */
   public HeaderInformation<?> readHeader(final Class<?> inheritFromClass)
   {
      byte firstByte;
      final int dimensionCount;
      final boolean primitiveArray;

      //excludes Object for the sake of Object[]
      if (null != inheritFromClass && !Object.class.equals(inheritFromClass))
      {
         //TODO: tests are likely thin
         //inheritFromClass is never primitive void.class.
         //It is only primitive if contained in a primitive array in which case there is no header
         //since it can't be null or any other class.
         if (inheritFromClass.isPrimitive())
            return HeaderInformation.forPrimitiveArrayValue(classUtil.boxClass(inheritFromClass).getName());
         //can't ignore header if inheritFromClass is final because it could be null (thus component will be either '?' or 0xFF)
         firstByte = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing header")[0];
         dimensionCount = arrayUtil.countArrayDimensions(inheritFromClass);
         final Class<?> baseComponent = inheritFromClass.isArray()
            ? arrayUtil.getBaseComponentType(inheritFromClass)
            : inheritFromClass;
         primitiveArray = baseComponent.isPrimitive();
         if ('?' == firstByte)
         {
            return HeaderInformation.forPossibleArray(firstByte, baseComponent.getName(), dimensionCount, primitiveArray);
         }
         //if inheritFromClass isn't primitive then it is not required to inherit type (eg null or child class) and continues below
      }
      else
      {
         firstByte = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Missing header")[0];
         if ('?' == firstByte) throw new StreamCorruptedException("Only array elements can inherit type");
         primitiveArray = (']' == firstByte);  //is false if not an array at all
         if ('[' == firstByte || ']' == firstByte)
         {
            dimensionCount = Byte.toUnsignedInt(
               StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Incomplete header: no array dimensions")[0]
            );
            //not the first byte but needs to conform for below. don't know what else to call this variable
            firstByte = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Incomplete header: no array component type")[0];
            if (StringSerializableStrategy.TERMINATOR == firstByte)
               throw new StreamCorruptedException("header's array component type can't be null");
            if ('-' == firstByte) throw new StreamCorruptedException("header's array component type can't be false");
            if ('+' == firstByte)
               return HeaderInformation.forPossibleArray(firstByte, Boolean.class.getName(), dimensionCount, primitiveArray);
         }
         else dimensionCount = 0;
      }

      if (StringSerializableStrategy.TERMINATOR == firstByte)
         return HeaderInformation.forNull(firstByte);  //the empty string class name means null
      if ('+' == firstByte) return HeaderInformation.forValue(firstByte, Boolean.class.getName(), Boolean.TRUE);
      if ('-' == firstByte) return HeaderInformation.forValue(firstByte, Boolean.class.getName(), Boolean.FALSE);
      if (COMPRESSED_HEADER_TO_CLASS.containsKey((char) firstByte))  //safe cast because map contains only ASCII
      {
         final Class<?> compressedClass = COMPRESSED_HEADER_TO_CLASS.get((char) firstByte);  //safe cast because map contains only ASCII
         return HeaderInformation.forPossibleArray(firstByte, compressedClass.getName(), dimensionCount, primitiveArray);
      }
      if ('&' == firstByte)
      {
         final int id = integerSerializableStrategy.read("Incomplete header: id type but no id");
         final Object registeredObject = readerRegistry.getRegisteredObject(id);
         //null value will not have an id. null is only possible if id was reserved but not registered
         if (registeredObject == null) throw new StreamCorruptedException("id not found");
         //LOG.debug("data.class=" + registeredObject.getClass().getSimpleName() + " val=" + registeredObject + " id=" + id);
         LOG.debug("id: " + id + " (" + registeredObject + " " + registeredObject.getClass().getSimpleName() + ")");
         return HeaderInformation.forValue(firstByte, registeredObject.getClass().getName(), registeredObject);
      }

      //else firstByte is part of a class name
      String className = "" + ((char) firstByte) + stringSerializableStrategy.read(null);
      return HeaderInformation.forPossibleArray(firstByte, className, dimensionCount, primitiveArray);
   }

   //TODO: rename since true also for null, bool

   /**
    * @return true if an id was used and no value should be written
    */
   public boolean writeHeaderReturnIsId(final Class<?> inheritFromClass, final Object data)
   {
      if (data != null && !classUtil.isPrimitiveOrBox(data.getClass()))
      {
         //TODO: long gets id, other primitives don't, else do
         final Integer id = writerRegistry.getId(data);
         //LOG.debug("data.class=" + data.getClass().getSimpleName() + " val=" + data + " id=" + id);
         if (id != null)
         {
            LOG.debug("id: " + id + " (" + data + " " + data.getClass().getSimpleName() + ")");
            byteSerializableStrategy.writeByte('&');
            integerSerializableStrategy.write(id);
            return true;
         }
         //null, primitive, and box don't get registered
         writerRegistry.registerObject(data);
      }
      //else if(data==null) LOG.debug("data.class=null");
      //else LOG.debug("data.class=" + data.getClass().getSimpleName() + " val=" + data);

      //boolean[] and Boolean[] use only headers for elements (primitive doesn't allow null)
      if (Boolean.TRUE.equals(data))
      {
         byteSerializableStrategy.writeByte('+');
         return true;
      }
      else if (Boolean.FALSE.equals(data))
      {
         byteSerializableStrategy.writeByte('-');
         return true;
      }
      else if (data == null)
      {
         //if data is null then class name is the empty string
         byteSerializableStrategy.writeByte(StringSerializableStrategy.TERMINATOR);
         return true;
      }
      //do nothing because non-boolean primitive array elements have no header
      else if (null != inheritFromClass && inheritFromClass.isPrimitive()) ;
         //(below) if class matches containing array exactly then inherit type.
      else if (null != inheritFromClass && inheritFromClass.equals(data.getClass())) byteSerializableStrategy.writeByte('?');
      else if (CLASS_TO_COMPRESSED_HEADER.containsKey(data.getClass()))
         byteSerializableStrategy.writeByte(CLASS_TO_COMPRESSED_HEADER.get(data.getClass()));
      else if (data.getClass().isArray())
      {
         Class<?> baseComponent = arrayUtil.getBaseComponentType(data.getClass());
         if (Object.class.equals(inheritFromClass) || null == inheritFromClass)
         {
            //array indicator and dimension count can be derived from containing array so don't populate it
            //unless I'm inside Object[] in which case I could have new arrays.
            //baseComponent can't be void or null
            if (baseComponent.isPrimitive())
            {
               byteSerializableStrategy.writeByte(']');
               baseComponent = classUtil.boxClass(baseComponent);
            }
            else byteSerializableStrategy.writeByte('[');

            final int dimensionCount = arrayUtil.countArrayDimensions(data.getClass());
            byteSerializableStrategy.writeByte(dimensionCount);  //won't be 0, max: 255. Use unsigned byte
         }

         if (baseComponent.equals(Boolean.class)) byteSerializableStrategy.writeByte('+');
         else if (CLASS_TO_COMPRESSED_HEADER.containsKey(baseComponent))
            byteSerializableStrategy.writeByte(CLASS_TO_COMPRESSED_HEADER.get(baseComponent));
         else
         {
            stringSerializableStrategy.write(baseComponent.getName());
         }
      }
      else
      {
         stringSerializableStrategy.write(data.getClass().getName());
      }

      return false;
   }
}
