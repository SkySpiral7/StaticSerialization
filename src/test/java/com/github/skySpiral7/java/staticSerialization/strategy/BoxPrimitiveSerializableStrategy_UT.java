package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.File;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class BoxPrimitiveSerializableStrategy_UT
{
   @Test
   public void read_throws_givenANonBooleanValue() throws Exception
   {
      final File tempFile = File.createTempFile("BoxPrimitiveSerializableStrategy_UT.TempFile.read_throws_givenANonBooleanValue.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, new byte[]{']', 1, '+'});  //array indicator, dimensions, component
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 1});  //length (int)
      FileIoUtil.appendToFile(tempFile, "a");  //not valid
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

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
   public void read_throws_givenStreamWithNullInPrimitiveBooleanArray() throws Exception
   {
      final File tempFile = File.createTempFile("BoxPrimitiveSerializableStrategy_UT.TempFile.readHeader_throws_whenInheritOutsideOfArray.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, new byte[]{']', 1, '+'});  //array indicator, dimensions, component
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 1});  //length (int)
      FileIoUtil.appendToFile(tempFile, ";");  //data is null rather than true/false
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

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
