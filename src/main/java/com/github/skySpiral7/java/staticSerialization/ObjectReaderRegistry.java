package com.github.skySpiral7.java.staticSerialization;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectReaderRegistry
{
   private static final Logger LOG = LogManager.getLogger();
   private final List<Object> registry = new ArrayList<>();
   /**
    * Used to tell if an object is already registered. Key: objects registered. Value: always true
    */
   private final Map<Object, Boolean> uniqueness = new IdentityHashMap<>();
   //TODO: doc: do not combine

   public void reserveIdForLater()
   {
      registry.add(null);
      LOG.debug(registry.size() - 1);
   }

   //TODO: I could make both registry internal and only have a method for ObjectStreamReader.registerObject
   public void registerObject(final Object instance)
   {
      //TODO: how does this work? it looks like reserving is required but it isn't always done? trace each flow
      Objects.requireNonNull(instance);
      //uniqueness is needed because of ArraySerializableStrategy
      //TODO: how is ArraySerializableStrategy the only case? add test
      if (!uniqueness.containsKey(instance))
      {
         //last index acts as a stack
         final int id = registry.lastIndexOf(null);
         if (id == -1) throw new StreamCorruptedException("id not found");
         registry.set(id, instance);
         LOG.debug(id + ": " + instance + " " + instance.getClass().getSimpleName());
         uniqueness.put(instance, Boolean.TRUE);
      }
   }

   public <T> T getRegisteredObject(final int id)
   {
      return ClassUtil.cast(registry.get(id));
   }
}
