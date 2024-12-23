package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import mockit.Expectations;
import mockit.FullVerifications;
import mockit.Mocked;
import org.junit.jupiter.api.Test;

import java.util.BitSet;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

class BitSetSerializableStrategy_UT
{
   @Test
   public void compress_matches_whenRoundTrip()
   {
      final BitSet data = BitSet.valueOf(new byte[]{
         (byte) 0xba, (byte) 0xbe
      });
      final byte[] compressed = BitSetSerializableStrategy.compress(data);
      assertEquals(data, BitSetSerializableStrategy.decompress(compressed));
   }

   @Test
   public void writeData(@Mocked final BoxPrimitiveSerializableStrategy mockBoxPrimitiveSerializableStrategy,
                         @Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final BitSetSerializableStrategy testObject =
         new BitSetSerializableStrategy(mockBoxPrimitiveSerializableStrategy, mockIntegerSerializableStrategy);
      final BitSet data = BitSet.valueOf(new byte[]{
         (byte) 0xba, (byte) 0xbe
      });

      testObject.writeData(data);

      new FullVerifications()
      {{
         mockIntegerSerializableStrategy.write(2);
         mockBoxPrimitiveSerializableStrategy.writeData((byte) 0xba);
         mockBoxPrimitiveSerializableStrategy.writeData((byte) 0xbe);
      }};
   }

   @Test
   public void readData(@Mocked final BoxPrimitiveSerializableStrategy mockBoxPrimitiveSerializableStrategy,
                        @Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final BitSetSerializableStrategy testObject =
         new BitSetSerializableStrategy(mockBoxPrimitiveSerializableStrategy, mockIntegerSerializableStrategy);
      final BitSet expected = BitSet.valueOf(new byte[]{
         (byte) 0xba, (byte) 0xbe
      });

      new Expectations()
      {{
         mockIntegerSerializableStrategy.read("Missing array length");
         result = 2;

         mockBoxPrimitiveSerializableStrategy.readData(Byte.class);
         returns((byte) 0xba, (byte) 0xbe);
      }};

      final Object actual = testObject.readData(null);

      assertEquals(expected, actual);
   }

   @Test
   public void e2e()
   {
      final BitSet data = BitSet.valueOf(new byte[]{
         (byte) 0xba, (byte) 0xbe
      });

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.util.BitSet");
      expectedBuilder.append(new byte[]{
         StringSerializableStrategy.TERMINATOR,
         0, 0, 0, 2,  //array length
         (byte) 0xba, (byte) 0xbe
      });
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertEquals(data, reader.readObject(BitSet.class));
      reader.close();
   }
}
