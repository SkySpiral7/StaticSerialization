package com.github.skySpiral7.java.staticSerialization;

import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * <p>Goals of this class in order:</p>
 * <ol>
 * <li>Security</li>
 * <li>Versatility</li>
 * <li>Nice API</li>
 * <li>Maintainability</li>
 * <li>Small serialized size</li>
 * </ol>
 * <p>Efficiency is not a goal. I haven't bench marked and I don't care.</p>
 * <p>A header that contains the class will be included with each element for security.
 * Most classes don't need a version number so one is not included by default.
 * Most classes don't need an id so one is not included by default.</p>
 *
 * <p>Note that using this interface also requires the class to define a static method of this signature:</p>
 * <blockQuote>{@code public static YourClass readFromStream(final ObjectStreamReader reader)}</blockQuote>
 */
//security holes: https://tersesystems.com/blog/2015/11/08/closing-the-open-door-of-java-object-serialization/
public interface StaticSerializable
{
   //public static T<this> readFromStream(final ObjectStreamReader reader)

   /**
    * If for some reason your parent class implements this interface but you don't want to implement it then
    * simply throw NotSerializableException here and don't define a readFromStream method.
    */
   public void writeToStream(final ObjectStreamWriter writer);

   /**
    * This static method includes the boilerplate code that is required if your class needs an id in order to avoid
    * a circular reference. Note that this method assumes that any object the stream asks to create will be created and returned,
    * if you want to return the same instance rather than a new one (despite the stream) then you must implement this yourself.
    *
    * @param reader      the current stream to read from
    * @param createEmpty a function (likely a method reference) that creates the object with minimal information
    * @param populate    a consumer (likely a method reference) that populates all other data within the object
    *
    * @return the already created object that matches the id or the object created by createEmpty
    */
   public static <T> T readFromStream(final ObjectStreamReader reader, final Function<ObjectStreamReader, T> createEmpty,
                                      final BiConsumer<ObjectStreamReader, T> populate)
   {
      final ObjectReaderRegistry registry = reader.getObjectRegistry();
      final T registeredObject = registry.readObjectOrId(reader);
      if (registeredObject != null) return registeredObject;

      final T result = createEmpty.apply(reader);
      registry.claimId(result);
      populate.accept(reader, result);
      return result;
   }
}
