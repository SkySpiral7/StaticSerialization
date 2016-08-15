package com.github.SkySpiral7.Java.serialization;

public interface StaticSerializable
{
	//public static T<this> readFromStream(ObjectInputStream in)
	public void writeToStream(ObjectOutputStream out);
}
