package com.github.SkySpiral7.Java.serialization;

import java.io.OutputStream;
import java.io.Serializable;

public class ObjectOutputStream extends OutputStream
{
	private final ObjectRegistry registry = new ObjectRegistry();

	@Override
	public void write(final int dataByte){}
	//@Override them all

	public void writeObject(final Object data){}  //for now ignore overloading for all primitives and array stuff
	public void writeSerializable(final Serializable data){}  //unchecked/unsafe and difficult to implement
	public void writeFieldsReflectively(final Object data){}

	public ObjectRegistry getObjectRegistry(){return registry;}
}
