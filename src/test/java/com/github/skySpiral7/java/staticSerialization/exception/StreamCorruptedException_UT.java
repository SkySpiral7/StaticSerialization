package com.github.skySpiral7.java.staticSerialization.exception;

import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

class StreamCorruptedException_UT
{
   @Test
   public void throwIfNotEnoughData_returns_whenMoreThanEnoughData()
   {
      final byte[] data = "hi".getBytes(StandardCharsets.UTF_8);
      final ByteReader byteReader = new ByteReader(data);

      byte[] actual = StreamCorruptedException.throwIfNotEnoughData(byteReader, 1, null);

      assertThat(actual, is(new byte[]{'h'}));
   }

   @Test
   public void throwIfNotEnoughData_returns_whenExactlyEnoughData()
   {
      final byte[] data = "hi".getBytes(StandardCharsets.UTF_8);
      final ByteReader byteReader = new ByteReader(data);

      byte[] actual = StreamCorruptedException.throwIfNotEnoughData(byteReader, 2, null);

      assertThat(actual, is(new byte[]{'h', 'i'}));
   }

   @Test
   public void throwIfNotEnoughData_throws_whenNotEnoughData()
   {
      final byte[] data = "hi".getBytes(StandardCharsets.UTF_8);
      final ByteReader byteReader = new ByteReader(data);
      final String myMessage = "test corruption";

      try
      {
         StreamCorruptedException.throwIfNotEnoughData(byteReader, 3, myMessage);
         fail("Should've thrown");
      }
      catch (StreamCorruptedException actual)
      {
         assertEquals(myMessage, actual.getMessage());
         assertEquals(NoMoreDataException.class, actual.getCause().getClass());
      }
   }

   @Test
   public void throwIfNotByteTerminated_returns_whenMultipleTerminators()
   {
      final byte[] data = "12,34,more".getBytes(StandardCharsets.UTF_8);
      final ByteReader byteReader = new ByteReader(data);

      byte[] actual = StreamCorruptedException.throwIfNotByteTerminated(byteReader, (byte) ',', null);

      assertThat(actual, is(new byte[]{'1', '2', ','}));
   }

   @Test
   public void throwIfNotByteTerminated_returns_whenEndsWithTerminator()
   {
      final byte[] data = "12,".getBytes(StandardCharsets.UTF_8);
      final ByteReader byteReader = new ByteReader(data);

      byte[] actual = StreamCorruptedException.throwIfNotByteTerminated(byteReader, (byte) ',', null);

      assertThat(actual, is(new byte[]{'1', '2', ','}));
   }

   @Test
   public void throwIfNotByteTerminated_throws_whenTerminatorNotFound()
   {
      final byte[] data = "hi".getBytes(StandardCharsets.UTF_8);
      final ByteReader byteReader = new ByteReader(data);
      final String myMessage = "test corruption";

      try
      {
         StreamCorruptedException.throwIfNotByteTerminated(byteReader, (byte) ',', myMessage);
         fail("Should've thrown");
      }
      catch (StreamCorruptedException actual)
      {
         assertEquals(myMessage, actual.getMessage());
      }
   }

   @Test
   public void throwIfNotByteTerminated_throws_whenEmpty()
   {
      final ByteReader byteReader = new ByteReader(new byte[0]);
      final String myMessage = "test corruption";

      try
      {
         StreamCorruptedException.throwIfNotByteTerminated(byteReader, (byte) ',', myMessage);
         fail("Should've thrown");
      }
      catch (StreamCorruptedException actual)
      {
         assertEquals(myMessage, actual.getMessage());
      }
   }
}
