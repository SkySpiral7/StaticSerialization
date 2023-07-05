package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

public class ArraySerializableStrategy_IT
{
   @Test
   public void objectArrayOfSelf()
   {
      //This test case exists to validate an edge case since Object[] is the only array that can contain itself
      final ByteAppender mockFile = new ByteAppender();
      final Object[] data = {1, 0};
      data[1] = data;

      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(mockFile.getAllBytes()));
      final Object[] actual = reader.readObject(Object[].class);
      reader.close();
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(actual));
      assertSame(actual, actual[1]);
   }
}
