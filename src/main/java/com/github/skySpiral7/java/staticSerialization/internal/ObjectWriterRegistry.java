package com.github.skySpiral7.java.staticSerialization.internal;

import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.IdentityHashMap;
import java.util.Map;

public class ObjectWriterRegistry
{
   private static final Logger LOG = LogManager.getLogger();
   /**
    * This could be an Identity list but there's no public class for that and map should be faster lookup. Key: objects registered. Value:
    * id
    */
   private final Map<Object, Integer> registry = new IdentityHashMap<>();
   private final ClassUtil classUtil;

   public ObjectWriterRegistry(ClassUtil classUtil) {
      this.classUtil = classUtil;
   }

   public void registerObject(final Object instance)
   {
      if (instance == null)
         LOG.info("Will not register null. Ignoring.");
      else if (registry.containsKey(instance))
         LOG.info("Already registered with id " + registry.get(instance) + ": " + instance + " " + instance.getClass().getSimpleName()+". Ignoring.");
      else if(classUtil.isPrimitiveOrBox(instance.getClass()) && !(instance instanceof Long) && !(instance instanceof Double))
         LOG.info("Will not register type "+instance.getClass()+". Ignoring.");
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
      if(instance == null) return null;
      return registry.get(instance);
   }
}
