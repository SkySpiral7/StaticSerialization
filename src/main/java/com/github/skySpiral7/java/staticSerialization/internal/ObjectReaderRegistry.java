package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class ObjectReaderRegistry
{
   private static final Logger LOG = LogManager.getLogger();
   private final List<Object> registry = new ArrayList<>();
   /**
    * Used to tell if an object is already registered. This is the reverse of the registry. Key: objects registered. Value: id
    * Not redundant with {@link #registry} because this needs to be identity based.
    */
   private final Map<Object, Integer> uniqueness = new IdentityHashMap<>();

   public void reserveIdForLater()
   {
      registry.add(null);
      LOG.debug(registry.size() - 1);
   }

   public boolean isRegistered(final Object instance)
   {
      Objects.requireNonNull(instance);
      return uniqueness.containsKey(instance);
   }

   public void registerObject(final Object instance)
   {
      Objects.requireNonNull(instance);
      /*
      reserveIdForLater is always called first since entry point is ObjectStreamReader.readObject.
      These are all cases where registerObject is called outside of InternalStreamReader.readObjectInternal:
      1) ObjectStreamReader.readFieldsReflectively
      2) StaticSerializable.readFromStream
      3) ArraySerializableStrategy.read
      4) GraphCallsRegister.Node.readFromStream

      Therefore, this will not be logged if using the library correctly however it isn't a warn since there's no harm.
      */
      if (uniqueness.containsKey(instance))
         LOG.info("Already registered with id " + uniqueness.get(instance) + ": " + instance + " " + instance.getClass().getSimpleName());
      else
      {
         //last index in order to make list LIFO
         final int id = registry.lastIndexOf(null);
         /*
         since reserveIdForLater is always called the only 2 ways this is possible:
         1) registerObject was manually called too many times
         2) Class's readFromStream was called directly
         */
         if (id == -1) throw new IllegalStateException("id not found. Make sure registerObject is only called for the "
            + "root object and that ObjectStreamReader.readObject etc are used as an "
            + "entry point for reading the stream.");
         registry.set(id, instance);
         LOG.debug(id + ": " + instance + " " + instance.getClass().getSimpleName());
         uniqueness.put(instance, id);
      }
   }

   public <T> T getRegisteredObject(final int id)
   {
      if (id < 0 || id >= registry.size())
         throw new StreamCorruptedException("invalid id. registry.size=" + registry.size() + " but found id=" + id);
      return ClassUtil.cast(registry.get(id));
   }
}
