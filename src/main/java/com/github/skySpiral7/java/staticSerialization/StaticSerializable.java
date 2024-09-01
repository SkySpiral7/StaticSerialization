package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;

import java.io.Externalizable;
import java.io.Serializable;
import java.util.EnumSet;
import java.util.function.BiConsumer;
import java.util.function.Function;

/**
 * <p>Goals of this library in order:</p>
 * <ol>
 * <li>Security</li>
 * <li>Versatility</li>
 * <li>Nice API</li>
 * <li>Small serialized size</li>
 * <li>Maintainability</li>
 * </ol>
 *
 * <p>Speed and RAM footprint are not goals. I haven't bench marked and I don't care.
 * However this library doesn't require setting fields with reflection (for security reasons)
 * therefore it is comparable to {@link Externalizable}.</p>
 *
 * <p>Note that implementing this interface also requires the class to define a static method of this signature:</p>
 * <blockQuote>{@code public static <YourClass> readFromStream(final ObjectStreamReader reader)}</blockQuote>
 *
 * <p>Usage: do not call readFromStream/writeToStream yourself as doing so will ruin the required overhead.</p>
 *
 * <p>A header that contains the class name will be included with each element for security.
 * Most classes don't need a version number so one is not included by default.
 * Most classes don't need an id but it will be included as needed
 * although classes with circular references require an extra step, see {@link #readFromStream(ObjectStreamReader, Function, BiConsumer)} for details.</p>
 *
 * <p>If you want a proxy (like {@link EnumSet} does) has then have each destination class implement a readFromStream
 * which calls a single common method. Likewise each writeToStream must be compatible.</p>
 *
 * <p>Java's {@link Serializable} doesn't allow for an enum to serialize fields even though enums can be
 * mutable. StaticSerializable does allow serializing enum field data (as per versatility) even though a mutable
 * singleton is a dependency injection smell.</p>
 *
 * <p>Likewise Java's {@link Serializable} doesn't allow custom serialization methods for record classes. There's not
 * much reason to have custom serialization for record classes but StaticSerializable does allow it.</p>
 *
 * <p>Java's {@link Serializable} doesn't preserve a record class appearing in multiple places in an object graph.
 * There's really no reason for that restriction so I consider that to be a bug. StaticSerializable does allow proper
 * backreferencing of record objects.</p>
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
    * <p>This static method includes the boilerplate code that is required if your class needs an id in order to avoid a circular reference.
    * Most classes don't need to register since circular references are not typical.
    * Note that this method assumes that any object the stream asks to create will be created and returned, if you want to return the same
    * instance rather than a new one (despite the stream) then you must implement this yourself.</p>
    *
    * <p>To be super clear what I mean by circular reference: an {@code Object[]} can contain itself. It is the only
    * array type that can contain itself (eg {@code Number[]} can't contain itself). {@code Object[]} has a circular reference
    * if it contains itself or contains something that in turn references back to the root. If your Bob class has a field
    * of type Bob then it is in danger of infinite recursion on serialization which this method exists to address.
    * However if Bob has a field of an {@code Object[]} which contains itself (but not a Bob) then Bob doesn't need
    * to register. Instead the class of {@code Object[]} would hypothetically need to register except that
    * {@code StaticSerializable} auto handles arrays.</p>
    *
    * <p>The "boilerplate" used is a single method call (to {@link ObjectStreamReader#registerObject(Object)})
    * so debatably calling this method actually requires more code than manually doing it yourself.
    * This method mainly exists to document how to resolve a circular reference. An advantage of actually calling this
    * method is that it better separates creation and population.</p>
    *
    * <p>{@link Serializable} doesn't require such a thing because the JVM uses an internal way to cheat the creation of
    * an object and store a reference to it before calling the read/write methods on it. {@code StaticSerializable}
    * on the other hand, abhors such black magic in favor of normal object creation in any way the class chooses including
    * the ability to choose not to create an object (thus not violating fixed instance invariants). However this means
    * that {@code StaticSerializable} doesn't have a reference to the new object until after the creation is done and thus
    * a method is sometimes needed to be inserted mid deserialization.</p>
    *
    * @param reader      the current stream to read from
    * @param createEmpty a function (likely a constructor reference) that creates the object with minimal data (possibly none)
    * @param populate    a consumer (likely a private method reference) that populates all other data within the object
    * @return a new object created by createEmpty. already registered and populated with data
    * @see ObjectStreamReader#registerObject(Object)
    */
   public static <T> T readFromStream(final ObjectStreamReader reader, final Function<ObjectStreamReader, T> createEmpty,
                                      final BiConsumer<ObjectStreamReader, T> populate)
   {
      final T result = createEmpty.apply(reader);
      reader.registerObject(result);
      populate.accept(reader, result);
      return result;
      //TODO: make less spiky: check correctness todos, maybe more tests
   }
}
