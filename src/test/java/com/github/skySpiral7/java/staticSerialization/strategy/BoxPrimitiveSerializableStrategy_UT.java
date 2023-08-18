package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class BoxPrimitiveSerializableStrategy_UT
{
   @Test
   public void read_throws_givenANonBooleanValue()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append(new byte[]{']', 1, '+'});  //array indicator, dimensions, component
      inputBuilder.append(new byte[]{0, 0, 0, 1});  //length (int)
      inputBuilder.append("a");  //not valid
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

      try
      {
         streamReader.readObject(boolean[].class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("97 is not a boolean value", actual.getMessage());
      }

      streamReader.close();
   }

   @Test
   public void read_throws_givenStreamWithNullInPrimitiveBooleanArray()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append(new byte[]{']', 1, '+'});  //array indicator, dimensions, component
      inputBuilder.append(new byte[]{0, 0, 0, 1});  //length (int)
      inputBuilder.append(";");  //data is null rather than true/false
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

      try
      {
         streamReader.readObject(boolean[].class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Primitive boolean array can't contain null", actual.getMessage());
      }

      streamReader.close();
   }
}
