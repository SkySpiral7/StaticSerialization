package com.github.SkySpiral7.Java.serialization;

import java.io.Closeable;
import java.io.Flushable;

public class ObjectInputStream implements Closeable, Flushable
{
	private final ObjectRegistry registry = new ObjectRegistry();

	/**
	 * Currently does nothing. Placeholder for later.
	 */
	@Override
   public void flush(){}
	/**
	 * Currently does nothing. Placeholder for later.
	 */
	@Override
   public void close(){}

	public boolean hasData(){return false;}  //can't call hasData(byte.class) because of overhead mismatch
	public boolean hasData(final Class<?> expectedClass){return false;}
	public int remainingBytes(){return 0;}

	public Object readObject(){return readObject(Object.class);}
	public <T> T readObject(final Class<T> expectedClass){return null;}
	public Object readSerializable(){return null;}  //unchecked/unsafe and difficult to implement
	public void readFieldsReflectively(final Object instance){}

	public ObjectRegistry getObjectRegistry(){return registry;}
}
