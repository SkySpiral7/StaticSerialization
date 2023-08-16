package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;

import java.io.Externalizable;
import java.util.EnumSet;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * <p>Goals of this library in order:</p>
 * <ol>
 * <li>Security</li>
 * <li>Versatility</li>
 * <li>Nice API</li>
 * <li>Maintainability</li>
 * <li>Small serialized size</li>
 * </ol>
 * <p>Speed and memory footprint are not goals. I haven't bench marked and I don't care.
 * However this library doesn't require setting fields with reflection (for security reasons)
 * therefore it is comparable to {@link Externalizable}.</p>
 * <p>A header that contains the class will be included with each element for security.
 * Most classes don't need a version number so one is not included by default.
 * Most classes don't need an id but it will be included as needed.</p>
 * <p>Usage: do not call these methods yourself as doing so will ruin the required overhead.</p>
 *
 * <p>Note that using this interface also requires the class to define a static method of this signature:</p>
 * <blockQuote>{@code public static YourClass readFromStream(final ObjectStreamReader reader)}</blockQuote>
 *
 * <p>If you want a proxy like {@link EnumSet} has then have each destination class implement a readFromStream
 * which calls a single common method. Likewise each writeToStream must be compatible.</p>
 *
 * @see ObjectStreamReader
 * @see ObjectStreamWriter
 */
//Java's security holes: https://tersesystems.com/blog/2015/11/08/closing-the-open-door-of-java-object-serialization/
public interface StaticSerializable
{
   //public static T<this> readFromStream(final ObjectStreamReader reader)

   /**
    * If for some reason your parent class implements this interface but you don't want to implement it then simply throw {@link
    * NotSerializableException} here and don't define a readFromStream method.
    */
   public void writeToStream(final ObjectStreamWriter writer);

   /**
    * This static method includes the boilerplate code that is required if your class needs an id in order to avoid a circular reference.
    * Note that this method assumes that any object the stream asks to create will be created and returned, if you want to return the same
    * instance rather than a new one (despite the stream) then you must implement this yourself.
    *
    * @param reader      the current stream to read from
    * @param createEmpty a function (likely a constructor reference) that creates the object with minimal information
    * @param populate    a consumer (likely a private method reference) that populates all other data within the object
    * @return a new object created by createEmpty. already registered and populated with data
    */
   public static <T> T readFromStream(final ObjectStreamReader reader, final Function<ObjectStreamReader, T> createEmpty,
                                      final BiConsumer<ObjectStreamReader, T> populate)
   {
      //TODO: wouldn't need readObjectOrId but must always registerObject after create empty. is that ok?
      //at that point have 2 static methods for createEmpty and populate
      //does that even work? since another object may be written.
      //master works only if createEmpty doesn't call readObjectOrId (which should be allowed)
      //if an id is created before calling createEmpty then it would work as long as createEmpty is correct
      //reg: pull null in list and give #, var = #, serial, set #
      //if createEmpty vs populate are correctly separated can createEmpty ruin the id?
      //is it possible to package data so that there only needs to be 1 method?

      final T result = createEmpty.apply(reader);
      reader.registerObject(result);
      populate.accept(reader, result);
      return result;
      //TODO: make less spiky: check correctness todos, maybe more tests
   }
}
