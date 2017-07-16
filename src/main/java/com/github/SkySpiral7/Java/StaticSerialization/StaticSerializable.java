package com.github.SkySpiral7.Java.StaticSerialization;

public interface StaticSerializable
{
   //public static T<this> readFromStream(final ObjectStreamReader reader)
   public void writeToStream(final ObjectStreamWriter writer);
}
