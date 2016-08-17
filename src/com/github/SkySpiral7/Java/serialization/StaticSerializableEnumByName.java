package com.github.SkySpiral7.Java.serialization;

public interface StaticSerializableEnumByName extends StaticSerializable
{
	public default void writeToStream(ObjectWriter out){}
}
