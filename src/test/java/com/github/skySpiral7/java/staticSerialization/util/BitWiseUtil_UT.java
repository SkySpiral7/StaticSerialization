package com.github.skySpiral7.java.staticSerialization.util;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class BitWiseUtil_UT
{
   private final BitWiseUtil testObject = new BitWiseUtil();

   @Test
   public void toBigEndianBytes()
   {
      final long input = 0x1234_5678__1234_5678L;

      final byte[] expected1Byte = {0x78};
      final byte[] actual1Byte = testObject.toBigEndianBytes(input, 1);
      assertEquals(Arrays.toString(expected1Byte), Arrays.toString(actual1Byte));

      final byte[] expected2Bytes = {0x56, 0x78};
      final byte[] actual2Bytes = testObject.toBigEndianBytes(input, 2);
      assertEquals(Arrays.toString(expected2Bytes), Arrays.toString(actual2Bytes));

      final byte[] expected3Bytes = {0x34, 0x56, 0x78};
      final byte[] actual3Bytes = testObject.toBigEndianBytes(input, 3);
      assertEquals(Arrays.toString(expected3Bytes), Arrays.toString(actual3Bytes));

      final byte[] expected4Bytes = {0x12, 0x34, 0x56, 0x78};
      final byte[] actual4Bytes = testObject.toBigEndianBytes(input, 4);
      assertEquals(Arrays.toString(expected4Bytes), Arrays.toString(actual4Bytes));

      final byte[] expected5Bytes = {0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual5Bytes = testObject.toBigEndianBytes(input, 5);
      assertEquals(Arrays.toString(expected5Bytes), Arrays.toString(actual5Bytes));

      final byte[] expected6Bytes = {0x56, 0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual6Bytes = testObject.toBigEndianBytes(input, 6);
      assertEquals(Arrays.toString(expected6Bytes), Arrays.toString(actual6Bytes));

      final byte[] expected7Bytes = {0x34, 0x56, 0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual7Bytes = testObject.toBigEndianBytes(input, 7);
      assertEquals(Arrays.toString(expected7Bytes), Arrays.toString(actual7Bytes));

      final byte[] expected8Bytes = {0x12, 0x34, 0x56, 0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual8Bytes = testObject.toBigEndianBytes(input, 8);
      assertEquals(Arrays.toString(expected8Bytes), Arrays.toString(actual8Bytes));

      //technically if you asked for more it would 0 pad but that's undefined behavior
   }

   @Test
   public void bigEndianBytesToInteger()
   {
      final int expected = 0x7234_5678;
      final byte[] inputBytes = {0x72, 0x34, 0x56, 0x78};
      final int actual = testObject.bigEndianBytesToInteger(inputBytes);
      assertEquals(expected, actual);
   }

   @Test
   public void bigEndianBytesToInteger_throws_givenWrongByteLength()
   {
      final byte[] input = {0, 0, 0x78};

      try
      {
         testObject.bigEndianBytesToInteger(input);
         fail("Should've thrown.");
      }
      catch (IllegalArgumentException actual)
      {
         assertEquals("expected length 4, got: 3", actual.getMessage());
      }
   }

   @Test
   public void bigEndianBytesToLong()
   {
      final long expected = 0x7234_5678__1234_5678L;
      final byte[] inputBytes = {0x72, 0x34, 0x56, 0x78, 0x12, 0x34, 0x56, 0x78};
      final long actual = testObject.bigEndianBytesToLong(inputBytes);
      assertEquals(expected, actual);
   }

   @Test
   public void bigEndianBytesToLong_throws_givenWrongByteLength()
   {
      final byte[] input = {0, 0, 0, 0, 0x78};

      try
      {
         testObject.bigEndianBytesToLong(input);
         fail("Should've thrown.");
      }
      catch (IllegalArgumentException actual)
      {
         assertEquals("expected length 8, got: 5", actual.getMessage());
      }
   }
}
