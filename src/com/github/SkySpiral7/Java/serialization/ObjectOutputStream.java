package com.github.SkySpiral7.Java.serialization;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.io.Writer;

import com.github.SkySpiral7.Java.util.FileIoUtil;

public class ObjectOutputStream extends OutputStream
{
	private final File destination;
	private final ObjectRegistry registry = new ObjectRegistry();

	public ObjectOutputStream(final File destination)
	{
		this.destination = destination;

		//start by clearing the file so that all writes can append (also this is fail fast to prove that writing is possible)
		FileIoUtil.writeToFile(destination, "");
	}

	@Override
	public void write(final int dataByte)
	{
		try
		{
			final Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(destination, true)));
			out.write(dataByte & 0xFF);
			out.close();
		}
		catch (final Exception e)
		{
			throw new RuntimeException(e);
		}
	}

	@Override
	public void write(final byte data[])
	{
		write(data, 0, data.length);  //I call mine instead of super to avoid try/catch wrapping
	}

	@Override
	public void write(final byte data[], final int startIndex, final int numberOfBytesToWrite)
	{
		try
		{
			super.write(data, startIndex, numberOfBytesToWrite);
		}
		catch (final IOException e)
		{
			throw new RuntimeException(e);
		}
	}
	@Override
   public void flush(){}  //overwritten so that there's no throws clause
	@Override
   public void close(){}  //and to remind me to use them later

	public void writeObject(final Object data){}  //for now ignore overloading for all primitives and array stuff
	public void writeSerializable(final Serializable data){}  //unchecked/unsafe and difficult to implement
	public void writeFieldsReflectively(final Object data){}

	public ObjectRegistry getObjectRegistry(){return registry;}
}
