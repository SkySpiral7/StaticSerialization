package com.github.skySpiral7.java.staticSerialization;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;

public class ObjectReaderRegistry
{
   private final List<Object> registry = new ArrayList<>();
   private final Map<Object, Boolean> uniqueness = new IdentityHashMap<>();
   //TODO: doc: do not combine

   public void registerObject(final Object instance)
   {
      Objects.requireNonNull(instance);
      //need to check if exists because arrays may have already been registered by the time internal read is done
      if (uniqueness.put(instance, Boolean.TRUE) == null) registry.add(instance);
   }

   public int getIdForLater()
   {
      registry.add(null);
      return registry.size() - 1;
   }

   public void registerLateObject(final Object instance, final int id)
   {
      Objects.requireNonNull(instance);
      registry.set(id, instance);
      uniqueness.put(instance, Boolean.TRUE);
   }

   public <T> T getRegisteredObject(final int id)
   {
      return ClassUtil.cast(registry.get(id));
   }
}
