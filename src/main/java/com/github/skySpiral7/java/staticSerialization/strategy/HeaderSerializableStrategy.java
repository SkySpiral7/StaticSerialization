package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
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
   private final Map<Class<?>, Character> CLASS_TO_COMPRESSED_HEADER;

   {
      /*
      possible printable ASCII headers: space to / (not $ or .) is 14, : to @ is +7, [ to ` (not _) is +5, { to ~ is +4 = 30
      I've used 14 so far which leaves 16 free spots
      forbidden: $.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz
      allowed (30): !"#%&'()*+,-/:;<=>?@[\]^`{|}~ space
      used (14): !"#%&'+-?@[]^~
      available (16): ()*,/:;<=>\`{|} space
      technically a FQ class name can't start with a number or dot so I could use them but I won't.
      variable names can start with $ so I assume a package/class can too
      */
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

   private final ObjectWriterRegistry writerRegistry;
   private final ArrayUtil arrayUtil;
   private final ClassUtil classUtil;
   private final AllSerializableStrategy allSerializableStrategy;
   private final ByteSerializableStrategy byteSerializableStrategy;
   private final IntegerSerializableStrategy integerSerializableStrategy;
   private final StringSerializableStrategy stringSerializableStrategy;

   public HeaderSerializableStrategy(final ObjectWriterRegistry registry,
                                     final UtilInstances utilInstances,
                                     final ByteSerializableStrategy byteSerializableStrategy,
                                     final IntegerSerializableStrategy integerSerializableStrategy,
                                     final StringSerializableStrategy stringSerializableStrategy)
   {
      writerRegistry = registry;
      arrayUtil = utilInstances.getArrayUtil();
      classUtil = utilInstances.getClassUtil();
      this.allSerializableStrategy = null;
      this.byteSerializableStrategy = byteSerializableStrategy;
      this.integerSerializableStrategy = integerSerializableStrategy;
      this.stringSerializableStrategy = stringSerializableStrategy;
   }

   public record PartialHeader(HeaderInformation<?> fullHeader, byte firstByte, int dimensionCount,
                                boolean primitiveArray) {}

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
         throw new IllegalStateException("Should not be called");
      }
      else if (Boolean.FALSE.equals(data))
      {
         throw new IllegalStateException("Should not be called");
      }
      else if (data == null)
      {
         throw new IllegalStateException("Should not be called");
      }
      //do nothing because non-boolean primitive array elements have no header
      else if (null != inheritFromClass && inheritFromClass.isPrimitive())
         throw new IllegalStateException("Should not be called");
         //(below) if class matches containing array exactly then inherit type.
      else if (null != inheritFromClass && inheritFromClass.equals(data.getClass()))
         byteSerializableStrategy.writeByte('?');
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
            stringSerializableStrategy.writeData(baseComponent.getName());
         }
      }
      else
      {
         stringSerializableStrategy.writeData(data.getClass().getName());
      }

      return false;
   }
}
