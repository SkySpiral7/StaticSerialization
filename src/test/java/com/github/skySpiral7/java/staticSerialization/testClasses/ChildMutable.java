package com.github.skySpiral7.java.staticSerialization.testClasses;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;

import java.util.Objects;

public class ChildMutable extends AbstractParent
{
   private String myData;

   public ChildMutable(String myData)
   {
      this.myData = myData;
   }

   @Override
   public void writeToStream(ObjectStreamWriter writer)
   {
      //bool: is mutable
      writer.writeObject(true);
      writer.writeObject(myData);
   }

   public static AbstractParent readFromStream(final ObjectStreamReader reader)
   {
      return AbstractParent.readFromStream(reader);
   }

   public String getMyData()
   {
      return myData;
   }

   public void setMyData(String myData)
   {
      this.myData = myData;
   }

   @Override
   public boolean equals(final Object obj)
   {
      if (!(obj instanceof ChildMutable)) return false;
      return Objects.equals(((ChildMutable) obj).myData, this.myData);
   }

   @Override
   public int hashCode()
   {
      return myData.hashCode();
   }

   @Override
   public String toString()
   {
      return "I could mutate " + myData;
   }
}
