package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import mockit.FullVerifications;
import mockit.Mocked;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;

class TinyBinarySerializableStrategy_UT
{
   @Test
   public void supportsWritingHeader()
   {
      final TinyBinarySerializableStrategy testObject = new TinyBinarySerializableStrategy(null);

      assertFalse(testObject.supportsWritingHeader(null, null));
      assertFalse(testObject.supportsWritingHeader(null, 1));
      assertTrue(testObject.supportsWritingHeader(null, new byte[0]));
      assertTrue(testObject.supportsWritingHeader(null, new byte[2]));
      assertTrue(testObject.supportsWritingHeader(null, new byte[128]));
      assertTrue(testObject.supportsWritingHeader(null, new byte[255]));
      assertFalse(testObject.supportsWritingHeader(null, new byte[256]));
   }

   @Test
   public void writeData(@Mocked final EasyAppender mockAppender)
   {
      final TinyBinarySerializableStrategy testObject = new TinyBinarySerializableStrategy(mockAppender);
      final byte[] input = {0, 1};

      testObject.writeData(input);

      new FullVerifications()
      {{
         mockAppender.append((byte) 2);
         mockAppender.append(input);
      }};
   }

   @Test
   public void readData_returns_whenHasData()
   {
      final ByteReader byteReader = new ByteReader(new byte[]{2, 0, 1});
      final TinyBinarySerializableStrategy testObject = new TinyBinarySerializableStrategy(null, byteReader);
      final byte[] expected = {0, 1};

      final Object actual = testObject.readData(null, null);

      assertThat(actual, is(expected));
   }

   @Test
   public void readData_throws_whenNoLength()
   {
      final ByteReader byteReader = new ByteReader(new byte[0]);
      final TinyBinarySerializableStrategy testObject = new TinyBinarySerializableStrategy(null, byteReader);

      try {
         testObject.readData(null, null);
         fail("Should throw");
      } catch (StreamCorruptedException actual) {
         assertEquals("Missing array length", actual.getMessage());
      }
   }

   @Test
   public void readData_throws_whenNotEnoughData()
   {
      final ByteReader byteReader = new ByteReader(new byte[]{2, 0});
      final TinyBinarySerializableStrategy testObject = new TinyBinarySerializableStrategy(null, byteReader);

      try {
         testObject.readData(null, null);
         fail("Should throw");
      } catch (StreamCorruptedException actual) {
         assertEquals("Not enough binary", actual.getMessage());
      }
   }

   @Test
   public void e2e_signed()
   {
      final byte[] data = {0, 1};

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{
        '=',  //header
         2,  //length
         0, 1  //data
      });
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertThat(reader.readObject(byte[].class), is(data));
      reader.close();
   }

   @Test
   public void e2e_unsigned()
   {
      final byte[] data = new byte[130];

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      assertThat(fileBytes[0], is((byte) '='));
      assertThat(0XFF & fileBytes[1], is(130));
      //ignore the rest of the bytes (all 0)

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertThat(reader.readObject(byte[].class), is(data));
      reader.close();
   }
}
