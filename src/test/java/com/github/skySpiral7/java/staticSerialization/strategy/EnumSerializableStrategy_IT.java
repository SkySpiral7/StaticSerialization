package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import org.junit.jupiter.api.Test;

import java.math.RoundingMode;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.fail;

public class EnumSerializableStrategy_IT
{
   private final BitWiseUtil bitWiseUtil = new BitWiseUtil();

   @Test
   public void write()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.math.RoundingMode");
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append(new byte[]{0, 0, 0, 1});
      final byte[] expected = expectedBuilder.getAllBytes();

      testObject.writeObject(RoundingMode.DOWN);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void read()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.math.RoundingMode");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      final byte[] fileContents = {0, 0, 0, 1};
      inputBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertSame(RoundingMode.DOWN, testObject.readObject(RoundingMode.class));

      testObject.close();
   }

   @Test
   public void read_throws_WhenOrdinalInvalid()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.math.RoundingMode");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append(bitWiseUtil.toBigEndianBytes(-1, 4));
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(RoundingMode.class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Invalid enum ordinal. Actual: -1", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void read_throws_WhenOrdinalNotFound()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.math.RoundingMode");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      final byte[] fileContents = {0, 0, 0, 10};
      inputBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
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
