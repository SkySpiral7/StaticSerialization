package com.github.skySpiral7.java.staticSerialization.internal;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Objects;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ObjectWriterRegistry
{
   /**
    * This could be an Identity list but there's no public class for that and map should be faster lookup. Key: objects registered. Value:
    * id
    */
   private final Map<Object, Integer> registry = new IdentityHashMap<>();
   private static final Logger LOG = LogManager.getLogger();

   public void registerObject(final Object instance)
   {
      Objects.requireNonNull(instance);
      if (registry.containsKey(instance))
         LOG.info("Already registered with id " + registry.get(instance) + ": " + instance + " " + instance.getClass().getSimpleName());
      else
      {
         //id will start 0 and after this method returns id is < size
         registry.put(instance, registry.size());
         LOG.debug((registry.size() - 1) + ": " + instance + " " + instance.getClass().getSimpleName());
      }
   }

   /**
    * @return the id of the instance or null if the object isn't registered
    */
   public Integer getId(final Object instance)
   {
      Objects.requireNonNull(instance);
      return registry.get(instance);
   }
}
