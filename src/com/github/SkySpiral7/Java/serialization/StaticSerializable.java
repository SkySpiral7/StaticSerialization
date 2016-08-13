package com.github.SkySpiral7.Java.serialization;

public interface StaticSerializable<T>
{
	public static boolean generateClassNameOverhead = true;

	//public static T readFromStream(ObjectInputStream in)
	public void writeToStream(ObjectOutputStream out);
}
