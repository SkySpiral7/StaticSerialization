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
//Java's security holes: https://tersesystems.com/blog/2015/11/08/closing-the-open-door-of-java-object-serialization/
//TODO: is it possible to have readFromStream return a child? in the case of EnumSet's proxy. doc the answer
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
      //TODO: wouldn't need readObjectOrId but must always registerObject after create empty. is that ok?
      //at that point have 2 static methods for createEmpty and populate
      //does that even work? since another object may be written.
      //master works only if createEmpty doesn't call readObjectOrId (which should be allowed)
      //if an id is created before calling createEmpty then it would work as long as createEmpty is correct
      //reg: pull null in list and give #, var = #, serial, set #
      //if createEmpty vs populate are correctly separated can createEmpty ruin the id?
      //is it possible to package data so that there only needs to be 1 method?

      //what about calling the static methods directly?
      //there's no way to return before create empty since id is in header.
      //I can't have these static read header because the header is read in order to know which static to call

      final ObjectReaderRegistry registry = reader.getObjectRegistry();
      final int id = registry.getIdForLater();
      final T result = createEmpty.apply(reader);
      registry.registerLateObject(result, id);
      populate.accept(reader, result);
      return result;

      /*
      Pro/con of uuid vs index for most vs circle for each call
      UUID:
         Both:
            Con: Calling static directly means no overhead which is bad for null
         Most:
            Pro: Have no id
            Pro: simple readFromStream without registry
            Con: Object[] have lots of ids for no reason even though Number[] etc don't
         Circle:
            Con: ids are large and exist as data after header
            Con: boilerplate important since there are a few steps
      Index:
         Both:
            Con: Calling static directly means no overhead which is bad for null and id
         Most:
            Pro: Have no id (even Object[]!)
            Pro: simple readFromStream without registry
         Circle:
            Pro: ids are small and exist as a header
            Pro: ids are rare
            Con: boilerplate exists but is simple
      */
   }
}
