package com.github.skySpiral7.java.staticSerialization.testClasses;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;

public class ChildImmutable extends AbstractParent
{
   private final int myData;

   public ChildImmutable(int myData)
   {
      this.myData = myData;
   }

   @Override
   public void writeToStream(ObjectStreamWriter writer)
   {
      //bool: is mutable
      writer.writeObject(false);
      writer.writeObject(myData);
   }

   public static AbstractParent readFromStream(final ObjectStreamReader reader)
   {
      return AbstractParent.readFromStream(reader);
   }

   public int getMyData()
   {
      return myData;
   }

   @Override
   public boolean equals(final Object obj)
   {
      if (!(obj instanceof ChildImmutable)) return false;
      return ((ChildImmutable) obj).myData == this.myData;
   }

   @Override
   public int hashCode()
   {
      return myData;
   }

   @Override
   public String toString()
   {
      return myData + " will never change";
   }
}
