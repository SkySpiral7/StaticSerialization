package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

public class ArraySerializableStrategy_IT
{
   @Test
   public void objectArrayOfSelf() throws IOException
   {
      //This test case exists to validate an edge case since Object[] is the only array that can contain itself
      final File tempFile = File.createTempFile("ArraySerializableStrategy_IT.TempFile.objectArrayOfSelf.", ".txt");
      tempFile.deleteOnExit();
      final Object[] data = {1, 0};
      data[1] = data;

      final ObjectStreamWriter writer = new ObjectStreamWriter(tempFile);
      writer.writeObject(data);
      writer.close();

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      final Object[] actual = reader.readObject(Object[].class);
      reader.close();
      assertEquals(Arrays.deepToString(data), Arrays.deepToString(actual));
      assertSame(actual, actual[1]);
   }
}
