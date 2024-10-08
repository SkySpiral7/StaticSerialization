package com.github.skySpiral7.java.staticSerialization.stream;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class ByteReader_UT
{

   @Test
   public void readBytes_returnsSome_whenRequestedLessThanAll()
   {
      final ByteReader testObject = new ByteReader("hi".getBytes(StandardCharsets.UTF_8));

      assertEquals("h", new String(testObject.readBytes(1), StandardCharsets.UTF_8));
      assertEquals("i", new String(testObject.readBytes(1), StandardCharsets.UTF_8));
   }

   @Test
   public void readBytes_returnsAll_whenExact()
   {
      final ByteReader testObject = new ByteReader("hi".getBytes(StandardCharsets.UTF_8));

      assertEquals("hi", new String(testObject.readBytes(2), StandardCharsets.UTF_8));
   }

   @Test
   public void readBytes_returnsShort_whenNotEnoughLeft()
   {
      final ByteReader testObject = new ByteReader("hi".getBytes(StandardCharsets.UTF_8));

      assertEquals("hi", new String(testObject.readBytes(3), StandardCharsets.UTF_8));
   }

   @Test
   public void readBytes_returnsEmpty_whenEmpty()
   {
      final ByteReader testObject = new ByteReader(new byte[0]);

      assertThat(testObject.readBytes(3), is(new byte[0]));
   }

   @Test
   public void readBytesUntil_returnsSome_whenRequestedLessThanAll()
   {
      final ByteReader testObject = new ByteReader("12,34,".getBytes(StandardCharsets.UTF_8));

      assertEquals("12,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
      assertEquals("34,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
   }

   @Test
   public void readBytesUntil_returnsAll_whenExact()
   {
      final ByteReader testObject = new ByteReader("hi,".getBytes(StandardCharsets.UTF_8));

      assertEquals("hi,", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
   }

   @Test
   public void readBytesUntil_returnsShort_whenNotEnoughLeft()
   {
      final ByteReader testObject = new ByteReader("hi".getBytes(StandardCharsets.UTF_8));

      assertEquals("hi", new String(testObject.readBytesUntil((byte) ','), StandardCharsets.UTF_8));
   }

   @Test
   public void readBytesUntil_returnsEmpty_whenEmpty()
   {
      final ByteReader testObject = new ByteReader(new byte[0]);

      assertThat(testObject.readBytesUntil((byte) ','), is(new byte[0]));
   }
}
