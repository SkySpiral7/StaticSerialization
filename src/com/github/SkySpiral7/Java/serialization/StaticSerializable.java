package com.github.SkySpiral7.Java.serialization;

public interface StaticSerializable
{
	//public static T<this> readFromStream(ObjectStreamReader reader)
	public void writeToStream(ObjectStreamWriter out);
}
