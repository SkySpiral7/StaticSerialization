package com.github.SkySpiral7.Java.serialization;

public interface StaticSerializableEnumByName extends StaticSerializable<Enum<?>>
{
	public default void writeToStream(ObjectOutputStream out){}
}
