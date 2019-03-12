package com.github.skySpiral7.java.staticSerialization;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

public class ObjectWriterRegistry
{
   private final Map<Object, Integer> registry = new IdentityHashMap<>();

   public void registerObject(final Object instance)
   {
      Objects.requireNonNull(instance);
      //id will start 0 and after this method returns id is < size
      registry.put(instance, registry.size());
   }

   /**
    * @return the id of the instance or null
    */
   public Integer getId(final Object instance)
   {
      Objects.requireNonNull(instance);
      return registry.get(instance);
   }
}
