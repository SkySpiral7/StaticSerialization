package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.strategy.ByteSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
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
   public void write(@Mocked final EasyAppender mockAppender, @Mocked final ByteSerializableStrategy mockByteSerializableStrategy)
   {
      final StringSerializableStrategy testObject = new StringSerializableStrategy(mockAppender, mockByteSerializableStrategy);
      final String input = "hi";

      testObject.write(input);

      new FullVerifications()
      {{
         mockAppender.append(new byte[]{'h', 'i'});
         mockByteSerializableStrategy.writeByte(StringSerializableStrategy.TERMINATOR);
      }};
   }

   @Test
   public void read_returns_whenHasData()
   {
      final ByteReader byteReader = new ByteReader(new byte[]{'h', 'i', StringSerializableStrategy.TERMINATOR});
      final StringSerializableStrategy testObject = new StringSerializableStrategy(null, byteReader);
      final String expected = "hi";

      final Object actual = testObject.read(null);

      assertEquals(expected, actual);
   }

   @Test
   public void read_throws_whenNotTerminated()
   {
      final ByteReader byteReader = new ByteReader(new byte[]{'h', 'i'});
      final StringSerializableStrategy testObject = new StringSerializableStrategy(null, byteReader);

      assertThrows(StreamCorruptedException.class, () -> testObject.read(null));
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
         '"',  //header
         'h', 'i',
         StringSerializableStrategy.TERMINATOR
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
      expectedBuilder.append((byte) '"');  //header
      expectedBuilder.append(HexFormat.of().parseHex("EFBFBD0041C3B1E2889EF09F98A2"));
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
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
         0, 0, 0, 0  //BitSet's array length
      });
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertEquals(data, reader.readObject(BitSet.class));
      reader.close();
   }
}
