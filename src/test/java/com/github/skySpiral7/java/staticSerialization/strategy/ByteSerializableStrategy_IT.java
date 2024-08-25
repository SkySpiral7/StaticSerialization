package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ByteSerializableStrategy_IT
{
   @Test
   public void write()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'~', 2, '~', 3});
      final byte[] expected = expectedBuilder.getAllBytes();

      Byte data1 = 2;
      byte data2 = 3;
      testObject.writeObject(data1);
      testObject.writeObject(data2);
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void read()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Byte;");
      inputBuilder.append(new byte[]{2, '~', 3});
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertEquals(2L, testObject.readObject(Byte.class).longValue());
      assertEquals(3L, testObject.readObject(byte.class).longValue());
      assertThat(mockFile.readBytes(1), is(new byte[0]));
   }
}
