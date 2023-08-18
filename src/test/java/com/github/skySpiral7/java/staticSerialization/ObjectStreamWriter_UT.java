package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ObjectStreamWriter_UT
{
   @Test
   public void writeFieldsReflectively()
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

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter_UT$1ReflectiveLocal;@");
      expectedBuilder.append(new byte[]{(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad});
      final byte[] expected = expectedBuilder.getAllBytes();

      testObject.writeObject(new ReflectiveLocal());
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }
}
