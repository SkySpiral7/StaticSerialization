package com.github.SkySpiral7.Java.serialization;

public interface StaticSerializable
{
	//public static T<this> readFromStream(final ObjectStreamReader reader)
	public void writeToStream(final ObjectStreamWriter writer);
}
