package com.github.SkySpiral7.Java.StaticSerialization;

/**
 * This interface should only be implemented by enums.
 * Use this interface if you want the enum to be serialized by the variable's name.
 * The order of the enum contants can change without breaking but the names must not
 * (case sensitive). This interface provides a definition for writeToStream (don't override it)
 * so that the enum only needs to implement this interface to receive functionality.
 *
 * @see StaticSerializable StaticSerializable for more details.
 * @see StaticSerializableEnumByOrdinal StaticSerializableEnumByOrdinal to use ordinal instead of name.
 */
public interface StaticSerializableEnumByName extends StaticSerializable
{
   /**
    * DO NOT CALL OR OVERRIDE THIS METHOD.
    * This method is ignored by ObjectStreamWriter so overriding it will
    * lead to unexpected behavior. I can't make this method final
    * and ObjectStreamWriter/Reader won't validate it. Because there's no reason to call
    * this method, it always throws (this maintains overhead and is consistent with the other read/writes).
    *
    * @throws IllegalStateException always
    * @see StaticSerializable use StaticSerializable directly for custom functionality
    */
   public default void writeToStream(final ObjectStreamWriter writer)
   {
      throw new IllegalStateException("Don't call this method.");
      //final Enum<?> self = (Enum<?>) this;
      //writer.writeObject(self.name());
      //except that doesn't write overhead and there's no way to read without overhead unless you roll your own
      //or I provide a non-standard static method here
      //but enums can't be subclassed so there's no reason to call this write method anyway
   }
}
