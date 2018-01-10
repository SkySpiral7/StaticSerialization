package com.github.skySpiral7.java.staticSerialization;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.strategy.JavaSerializableStrategy;
import com.github.skySpiral7.java.util.BitWiseUtil;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ObjectStreamWriter_UT
{
   @Test
   public void constructor_throws()
   {
      try
      {
         new ObjectStreamWriter(new File(".")).close();
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("It is not possible to write to a directory (.)", actual.getMessage());
      }
   }

   @Test
   public void constructor_clears() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.constructor_clears.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "test");
      new ObjectStreamWriter(tempFile).close();
      assertEquals("", FileIoUtil.readTextFile(tempFile));
   }

   @Test
   public void writeObject_enum() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_enum.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(RoundingMode.DOWN);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String header = "java.math.RoundingMode;";
      final byte[] data = {0, 0, 0, 1};
      assertEquals(header, bytesToString(fileContents, data.length));
      assertEquals(Arrays.toString(data), Arrays.toString(shortenBytes(fileContents, data.length)));
   }

   @Test
   public void writeObject_custom() throws IOException
   {
      final class CustomLocal implements StaticSerializable
      {
         boolean wasCalled = false;

         //no reader doesn't matter

         @Override
         public void writeToStream(ObjectStreamWriter out)
         {
            wasCalled = true;
         }
      }

      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_custom.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final CustomLocal data = new CustomLocal();

      testObject.writeObject(data);
      testObject.close();
      assertTrue(data.wasCalled);
   }

   private static enum CustomEnum implements StaticSerializable
   {
      One, Two;

      @Override
      public void writeToStream(final ObjectStreamWriter writer)
      {
         writer.writeObject(this.name());
      }
   }

   @Test
   public void writeObject_customEnum() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_customEnum.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(CustomEnum.One);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String header = "com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter_UT$CustomEnum;*";
      final byte[] data = {0, 0, 0, 3,  //UTF-8 length (int)
            79, 110, 101};  //"One"
      assertEquals(header, bytesToString(fileContents, data.length));
      assertEquals(Arrays.toString(data), Arrays.toString(shortenBytes(fileContents, data.length)));
   }

   @Test
   public void writeObject_Serializable() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_Serializable.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      final BigInteger data = BigInteger.TEN;
      final byte[] javaData = JavaSerializableStrategy.javaSerialize(data);

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("java.math.BigInteger;", bytesToString(fileContents, (javaData.length + 4)));

      final byte[] bytesOfSize = new byte[4];
      System.arraycopy(fileContents, "java.math.BigInteger;".length(), bytesOfSize, 0, 4);
      assertEquals(javaData.length, BitWiseUtil.bigEndianBytesToInteger(bytesOfSize));

      assertEquals(Arrays.toString(javaData), Arrays.toString(shortenBytes(fileContents, javaData.length)));
   }

   @Test
   public void writeObject_throw_unknownClass() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_throw_unknownClass.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      try
      {
         testObject.writeObject(new Object());
         fail("Didn't throw");
      }
      catch (final NotSerializableException actual)
      {
         assertEquals("java.lang.Object", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void writeFieldsReflectively() throws IOException
   {
      final class ReflectiveLocal implements StaticSerializable
      {
         private int field = 0xcafe_bead;

         //no reader doesn't matter

         @Override
         public void writeToStream(final ObjectStreamWriter writer)
         {
            writer.writeFieldsReflectively(this);
         }
      }

      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeFieldsReflectively.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final byte[] expected = {(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad};

      testObject.writeObject(new ReflectiveLocal());
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter_UT$1ReflectiveLocal;@",
            bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   private String bytesToString(final byte[] data, final int bytesToIgnore)
   {
      return new String(data, 0, (data.length - bytesToIgnore), StandardCharsets.UTF_8);
   }

   private byte[] shortenBytes(final byte[] data, final int bytesToKeep)
   {
      final byte[] smallerData = new byte[bytesToKeep];
      System.arraycopy(data, (data.length - bytesToKeep), smallerData, 0, bytesToKeep);
      return smallerData;
   }
}
