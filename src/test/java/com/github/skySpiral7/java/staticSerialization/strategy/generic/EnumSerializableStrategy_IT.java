package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import mockit.Expectations;
import mockit.FullVerifications;
import mockit.Mocked;
import org.junit.jupiter.api.Test;

import java.math.RoundingMode;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class EnumSerializableStrategy_IT
{
   @Test
   public void write(@Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final var testObject = new EnumSerializableStrategy(mockIntegerSerializableStrategy);

      testObject.write(RoundingMode.DOWN);

      new FullVerifications()
      {{
         mockIntegerSerializableStrategy.write(RoundingMode.DOWN.ordinal());
      }};
   }

   @Test
   public void read(@Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final var testObject = new EnumSerializableStrategy(mockIntegerSerializableStrategy);

      new Expectations()
      {{
         mockIntegerSerializableStrategy.read("Missing enum ordinal");
         result = 0;
      }};

      RoundingMode actual = testObject.read(RoundingMode.class);

      assertEquals(RoundingMode.UP, actual);
      new FullVerifications() {};
   }

   @Test
   public void read_throws_whenOrdinalInvalid(@Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final var testObject = new EnumSerializableStrategy(mockIntegerSerializableStrategy);

      new Expectations()
      {{
         mockIntegerSerializableStrategy.read("Missing enum ordinal");
         result = -1;
      }};

      try
      {
         testObject.read(RoundingMode.class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Invalid enum ordinal. Actual: -1", actual.getMessage());
      }
      new FullVerifications() {};
   }

   @Test
   public void read_throws_whenOrdinalNotFound(@Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final var testObject = new EnumSerializableStrategy(mockIntegerSerializableStrategy);

      new Expectations()
      {{
         mockIntegerSerializableStrategy.read("Missing enum ordinal");
         result = 10;
      }};

      try
      {
         testObject.read(RoundingMode.class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("java.math.RoundingMode.values()[10] doesn't exist. Actual length: 8", actual.getMessage());
      }
      new FullVerifications() {};
   }

   /**
    * For a test for custom enum see {@link StaticSerializableStrategyTest#e2e_staticEnum()} since it doesn't use the enum strat.
    */
   @Test
   public void e2e()
   {
      final RoundingMode data = RoundingMode.DOWN;

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.math.RoundingMode");
      expectedBuilder.append(new byte[]{
         StringSerializableStrategy.TERMINATOR,
         0, 0, 0, 1
      });
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertEquals(data, reader.readObject(RoundingMode.class));
      reader.close();
   }
}
