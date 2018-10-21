package com.github.skySpiral7.java.staticSerialization;

import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;

public class ObjectWriterRegistry
{
   private final Map<Object, String> registry = new IdentityHashMap<>();

   public String registerObject(final Object instance)
   {
      Objects.requireNonNull(instance);
      final String id = UUID.randomUUID().toString();
      //TODO: getLeastSignificantBits etc would save space
      registry.put(instance, id);
      return id;
   }

   public void registerObject(final String id, final Object instance)
   {
      Objects.requireNonNull(id);
      Objects.requireNonNull(instance);
      registry.put(instance, id);
   }

   /**
    * @return the id of the instance or null
    */
   public String getId(final Object instance)
   {
      Objects.requireNonNull(instance);
      return registry.get(instance);
   }

   /**
    * Writes the id of instance to the stream.
    *
    * @throws NullPointerException if either parameter is null or if instance isn't registered.
    */
   public void writeId(final Object instance, final ObjectStreamWriter writer)
   {
      Objects.requireNonNull(instance);
      Objects.requireNonNull(writer);
      final String id = registry.get(instance);
      Objects.requireNonNull(id);
      writer.writeObject(id);
   }

   /**
    * Writes the id of instance to the stream, registers instance (if needed),
    * then returns true if instance was already registered.
    */
   public boolean shouldNotWrite(final Object instance, final ObjectStreamWriter writer)
   {
      Objects.requireNonNull(instance);
      Objects.requireNonNull(writer);
      String id = registry.get(instance);
      if (id != null)
      {
         //if already exists then write the id and stop
         writer.writeObject(id);
         return true;
      }
      //else create an id, write it, and continue writing the object
      id = UUID.randomUUID().toString();
      registry.put(instance, id);
      writer.writeObject(id);
      return false;
   }
}
