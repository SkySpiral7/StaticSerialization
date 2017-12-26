package com.github.skySpiral7.java.staticSerialization.testClasses;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;

public final class SimpleHappy implements StaticSerializable
{
   public final int smileyStickersCount;

   public SimpleHappy(final int smileyStickersCount)
   {
      this.smileyStickersCount = smileyStickersCount;
   }

   public static SimpleHappy readFromStream(final ObjectStreamReader in)
   {
      return new SimpleHappy(in.readObject(int.class));
   }

   @Override
   public void writeToStream(final ObjectStreamWriter out)
   {
      out.writeObject(smileyStickersCount);
   }

   @Override
   public boolean equals(final Object obj)
   {
      if (!(obj instanceof SimpleHappy)) return false;
      return (SimpleHappy.class.cast(obj).smileyStickersCount == this.smileyStickersCount);
   }

   @Override
   public int hashCode()
   {
      return smileyStickersCount;
   }

   @Override
   public String toString()
   {
      return "I'm so happy that I own " + smileyStickersCount + " smiley stickers!";
   }
}
