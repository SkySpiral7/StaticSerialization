package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import mockit.Expectations;
import mockit.Mocked;
import mockit.Verifications;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

class UuidSerializableStrategy_UT
{
   @Test
   public void compress_matches_whenRoundTrip()
   {
      final UUID uuid = UUID.randomUUID();
      final long[] compressed = UuidSerializableStrategy.compress(uuid);
      //I guess the bytes include the version but doesn't matter as long as can round trip
      assertEquals(uuid, UuidSerializableStrategy.decompress(compressed));
   }

   @Test
   public void write(@Mocked final BoxPrimitiveSerializableStrategy mockBoxPrimitiveSerializableStrategy)
   {
      final UuidSerializableStrategy testObject = new UuidSerializableStrategy(mockBoxPrimitiveSerializableStrategy);
      final UUID input = new UUID(0xc14661197b9c40b0L, 0x8129caca6d2b15f1L);

      testObject.write(input);

      new Verifications()
      {{
         mockBoxPrimitiveSerializableStrategy.write(0xc14661197b9c40b0L);
         mockBoxPrimitiveSerializableStrategy.write(0x8129caca6d2b15f1L);
      }};
   }

   @Test
   public void read(@Mocked final BoxPrimitiveSerializableStrategy mockBoxPrimitiveSerializableStrategy)
   {
      final UuidSerializableStrategy testObject = new UuidSerializableStrategy(mockBoxPrimitiveSerializableStrategy);
      final UUID expected = new UUID(0xc14661197b9c40b0L, 0x8129caca6d2b15f1L);

      new Expectations()
      {{
         mockBoxPrimitiveSerializableStrategy.read(Long.class);
         returns(0xc14661197b9c40b0L, 0x8129caca6d2b15f1L);
      }};

      final Object actual = testObject.read(null);

      assertEquals(expected, actual);
   }

   @Test
   public void e2e()
   {
      final UUID data = new UUID(0xc14661197b9c40b0L, 0x8129caca6d2b15f1L);

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.util.UUID;");
      expectedBuilder.append(new byte[]{
         (byte) 0xc1, 0x46, 0x61, 0x19, 0x7b, (byte) 0x9c, 0x40, (byte) 0xb0,
         (byte) 0x81, 0x29, (byte) 0xca, (byte) 0xca, 0x6d, 0x2b, 0x15, (byte) 0xf1
      });
      assertArrayEquals(expectedBuilder.getAllBytes(), fileBytes);

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertEquals(data, reader.readObject(UUID.class));
      reader.close();
   }
}
