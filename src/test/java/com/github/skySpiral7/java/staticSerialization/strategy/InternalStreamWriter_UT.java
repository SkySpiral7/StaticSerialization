package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class InternalStreamWriter_UT
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
      final File tempFile = File.createTempFile("InternalStreamWriter_UT.TempFile.constructor_clears.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "test");
      new ObjectStreamWriter(tempFile).close();
      assertEquals("", FileIoUtil.readTextFile(tempFile));
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

      final File tempFile = File.createTempFile("InternalStreamWriter_UT.TempFile.writeObject_custom.", ".txt");
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
      final File tempFile = File.createTempFile("InternalStreamWriter_UT.TempFile.writeObject_customEnum.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(CustomEnum.class.getName().getBytes(StandardCharsets.UTF_8));
      baos.write(";*".getBytes(StandardCharsets.UTF_8));
      baos.write(new byte[]{0, 0, 0, 3});  //UTF-8 length (int)
      baos.write("One".getBytes(StandardCharsets.UTF_8));
      final byte[] expected = baos.toByteArray();

      testObject.writeObject(CustomEnum.One);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_Serializable() throws IOException
   {
      final File tempFile = File.createTempFile("InternalStreamWriter_UT.TempFile.writeObject_Serializable.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      final BigInteger data = BigInteger.TEN;
      final byte[] javaData = JavaSerializableStrategy.javaSerialize(data);
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write("java.math.BigInteger;".getBytes(StandardCharsets.UTF_8));
      baos.write(ByteSerializableStrategy.toBigEndianBytes(javaData.length, 4));
      baos.write(javaData);
      final byte[] expected = baos.toByteArray();

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_throw_unknownClass() throws IOException
   {
      final File tempFile = File.createTempFile("InternalStreamWriter_UT.TempFile.writeObject_throw_unknownClass.", ".txt");
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
}
