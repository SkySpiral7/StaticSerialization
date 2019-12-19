package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

public class EnumSerializableStrategy_IT
{
   @Test
   public void write() throws Exception
   {
      final File tempFile = File.createTempFile("EnumSerializableStrategy_IT.TempFile.write.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write("java.math.RoundingMode;".getBytes(StandardCharsets.UTF_8));
      baos.write(new byte[]{0, 0, 0, 1});
      final byte[] expected = baos.toByteArray();

      testObject.writeObject(RoundingMode.DOWN);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void read() throws Exception
   {
      final File tempFile = File.createTempFile("EnumSerializableStrategy_IT.TempFile.read.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.math.RoundingMode;");
      final byte[] fileContents = {0, 0, 0, 1};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertSame(RoundingMode.DOWN, testObject.readObject(RoundingMode.class));

      testObject.close();
   }

   @Test
   public void read_throws_WhenOrdinalInvalid() throws Exception
   {
      final File tempFile = File.createTempFile("EnumSerializableStrategy_IT.TempFile.read_throws_WhenOrdinalInvalid.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.math.RoundingMode;");
      FileIoUtil.appendToFile(tempFile, ByteSerializableStrategy.toBigEndianBytes(-1, 4));

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(RoundingMode.class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("expected array index. Actual: -1", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void read_throws_WhenOrdinalNotFound() throws Exception
   {
      final File tempFile = File.createTempFile("EnumSerializableStrategy_IT.TempFile.read_throws_WhenOrdinalNotFound.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.math.RoundingMode;");
      final byte[] fileContents = {0, 0, 0, 10};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(RoundingMode.class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("java.math.RoundingMode.values()[10] doesn't exist. Actual length: 8", actual.getMessage());
      }

      testObject.close();
   }
}
