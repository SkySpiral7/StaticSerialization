package com.github.SkySpiral7.Java.serialization;

/**
 * This interface should only be implemented by enums.
 * Use this interface if you want the enum to be serialized by the variable's ordinal.
 * The names of the enum contants can change without breaking but the order of them must not change.
 * This interface provides a definition for writeToStream (don't override it)
 * so that the enum only needs to implement this interface to receive functionality.
 * 
 * @see StaticSerializable StaticSerializable for more details.
 * @see StaticSerializableEnumByName StaticSerializableEnumByName to use name instead of ordinal.
 */
public interface StaticSerializableEnumByOrdinal extends StaticSerializable
{
	/**
	 * @see StaticSerializableEnumByName#writeToStream(ObjectWriter)
	 */
	public default void writeToStream(final ObjectWriter writer)
	{
		throw new IllegalStateException("Don't call this method.");
		//final Enum<?> self = (Enum<?>) this;
		//writer.writeObject(self.ordinal());
	}
}
