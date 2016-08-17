package com.github.SkySpiral7.Java.serialization.testClasses;

import com.github.SkySpiral7.Java.serialization.ObjectReader;
import com.github.SkySpiral7.Java.serialization.ObjectWriter;
import com.github.SkySpiral7.Java.serialization.StaticSerializable;

public final class SimpleHappy implements StaticSerializable
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

	@Override
	public boolean equals(final Object obj)
	{
		if (!(obj instanceof SimpleHappy)) return false;
		return obj.hashCode() == this.hashCode();
		//this is only ok because there are provably 0 collisions.
	}

	@Override
	public int hashCode()
	{
		return smilyStickersCount;
	}

	@Override
	public String toString()
	{
		return "I'm so happy that I own " + smilyStickersCount + " smily stickers!";
	}
}
