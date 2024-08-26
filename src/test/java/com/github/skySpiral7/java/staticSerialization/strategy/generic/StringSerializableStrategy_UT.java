package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.IntegerSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import mockit.Expectations;
import mockit.FullVerifications;
import mockit.Mocked;
import org.junit.jupiter.api.Test;

import java.util.BitSet;
import java.util.HexFormat;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class StringSerializableStrategy_UT
{
   @Test
   public void write(@Mocked final EasyAppender mockAppender,
                     @Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final StringSerializableStrategy testObject = new StringSerializableStrategy(mockAppender, null,
         mockIntegerSerializableStrategy);
      final String input = "hi";

      testObject.write(input);

      new FullVerifications()
      {{
         mockIntegerSerializableStrategy.write(2);
         mockAppender.append(new byte[]{'h', 'i'});
      }};
   }

   @Test
   public void read_returns_whenHasData(@Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final ByteReader byteReader = new ByteReader(new byte[]{'h', 'i'});
      final StringSerializableStrategy testObject = new StringSerializableStrategy(byteReader,
         mockIntegerSerializableStrategy);
      final String expected = "hi";

      new Expectations()
      {{
         mockIntegerSerializableStrategy.read("Missing string byte length");
         result = 2;
      }};

      final Object actual = testObject.read(null);

      assertEquals(expected, actual);
   }

   @Test
   public void read_throws_whenNotEnoughData(@Mocked final IntegerSerializableStrategy mockIntegerSerializableStrategy)
   {
      final ByteReader byteReader = new ByteReader(new byte[]{'h', 'i'});
      final StringSerializableStrategy testObject = new StringSerializableStrategy(byteReader,
         mockIntegerSerializableStrategy);

      new Expectations()
      {{
         mockIntegerSerializableStrategy.read("Missing string byte length");
         result = 4;
      }};

      assertThrows(StreamCorruptedException.class, () -> testObject.read(null));
   }

   @Test
   public void writeClassName(@Mocked final EasyAppender mockAppender,
                              @Mocked final ByteSerializableStrategy mockByteSerializableStrategy)
   {
      final StringSerializableStrategy testObject = new StringSerializableStrategy(mockAppender,
         mockByteSerializableStrategy, null);
      final String input = "hi";

      testObject.writeClassName(input);

      new FullVerifications()
      {{
         mockAppender.append(new byte[]{'h', 'i'});
         mockByteSerializableStrategy.writeByte(StringSerializableStrategy.TERMINATOR);
      }};
   }

   @Test
   public void readClassName_returns_whenHasTerminator()
   {
      final ByteReader byteReader = new ByteReader(new byte[]{'i', StringSerializableStrategy.TERMINATOR});
      final StringSerializableStrategy testObject = new StringSerializableStrategy(byteReader, null);
      final String expected = "hi";

      final Object actual = testObject.readClassName((byte) 'h');

      assertEquals(expected, actual);
   }

   @Test
   public void readClassName_throws_whenNoTerminator()
   {
      final ByteReader byteReader = new ByteReader(new byte[]{'i'});
      final StringSerializableStrategy testObject = new StringSerializableStrategy(byteReader, null);
      final String expected = "hi";

      assertThrows(StreamCorruptedException.class, () -> testObject.readClassName((byte) 'h'));
   }

   @Test
   public void e2e_stringData()
   {
      final String data = "hi";

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{
         '*',  //header
         0, 0, 0, 2,  //string byte length
         'h', 'i'
      });
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertEquals(data, reader.readObject(String.class));
      reader.close();
   }

   @Test
   public void e2e_stress()
   {
      /* This is a UTF-8 stress test:
       * EFBFBD: replacement character �
       * 00: null: edge case control character
       * 41: A: single byte ascii
       * C3B1: ñ 2 byte BMP
       * E2889E: ∞ 3 byte BMP (2 byte UTF-16BE)
       * F09F98A2: 😢 4 byte non-BMP */
      final String data = "�\u0000Añ∞😢";

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{
         '*',  //header
         0, 0, 0, 14  //string byte length
      });
      expectedBuilder.append(HexFormat.of().parseHex("EFBFBD0041C3B1E2889EF09F98A2"));
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertEquals(data, reader.readObject(String.class));
      reader.close();
   }

   @Test
   public void e2e_classData()
   {
      final BitSet data = BitSet.valueOf(new byte[0]);

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.util.BitSet");
      expectedBuilder.append(new byte[]{
         StringSerializableStrategy.TERMINATOR,
         0, 0, 0, 0  //array length
      });
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertEquals(data, reader.readObject(BitSet.class));
      reader.close();
   }
}
