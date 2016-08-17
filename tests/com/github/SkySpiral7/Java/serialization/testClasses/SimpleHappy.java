package com.github.SkySpiral7.Java.serialization.testClasses;

import com.github.SkySpiral7.Java.serialization.ObjectReader;
import com.github.SkySpiral7.Java.serialization.ObjectWriter;
import com.github.SkySpiral7.Java.serialization.StaticSerializable;

public class SimpleHappy implements StaticSerializable
{
	public final int smilyStickersCount;

	public SimpleHappy(final int smilyStickersCount)
	{
		this.smilyStickersCount = smilyStickersCount;
	}

	public static SimpleHappy readFromStream(final ObjectReader in)
	{
		return new SimpleHappy(in.readObject(int.class));
	}

	@Override
	public void writeToStream(final ObjectWriter out)
	{
		out.writeObject(smilyStickersCount);
	}
}
