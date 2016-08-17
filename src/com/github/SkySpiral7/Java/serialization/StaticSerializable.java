package com.github.SkySpiral7.Java.serialization;

public interface StaticSerializable
{
	//public static T<this> readFromStream(ObjectReader reader)
	public void writeToStream(ObjectWriter out);
}
