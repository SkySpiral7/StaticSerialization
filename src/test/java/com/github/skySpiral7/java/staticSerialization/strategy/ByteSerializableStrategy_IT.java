package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ByteSerializableStrategy_IT
{
   @Test
   public void write() throws Exception
   {
      final File tempFile = File.createTempFile("ByteSerializableStrategy_IT.TempFile.write.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'~', 2, '~', 3});
      final byte[] expected = baos.toByteArray();

      Byte data1 = 2;
      byte data2 = 3;
      testObject.writeObject(data1);
      testObject.writeObject(data2);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void read() throws Exception
   {
      final File tempFile = File.createTempFile("ByteSerializableStrategy_IT.TempFile.read.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Byte;");
      FileIoUtil.appendToFile(tempFile, new byte[]{2, '~', 3});

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      assertEquals(2L, testObject.readObject(Byte.class).longValue());
      assertEquals(3L, testObject.readObject(byte.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }
}
