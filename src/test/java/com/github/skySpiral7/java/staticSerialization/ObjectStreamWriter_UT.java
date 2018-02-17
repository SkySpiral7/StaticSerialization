package com.github.skySpiral7.java.staticSerialization;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ObjectStreamWriter_UT
{
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
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(
            "com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter_UT$1ReflectiveLocal;@".getBytes(StandardCharsets.UTF_8));
      baos.write(new byte[]{(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad});
      final byte[] expected = baos.toByteArray();

      testObject.writeObject(new ReflectiveLocal());
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }
}
