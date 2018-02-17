package com.github.skySpiral7.java.staticSerialization.strategy;

import java.util.Arrays;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class ByteSerializableStrategy_UT
{
   @Test
   public void toBigEndianBytes()
   {
      final long input = 0x1234_5678__1234_5678L;

      final byte[] expected1Byte = {0x78};
      final byte[] actual1Byte = ByteSerializableStrategy.toBigEndianBytes(input, 1);
      assertEquals(Arrays.toString(expected1Byte), Arrays.toString(actual1Byte));

      final byte[] expected2Bytes = {0x56, 0x78};
      final byte[] actual2Bytes = ByteSerializableStrategy.toBigEndianBytes(input, 2);
      assertEquals(Arrays.toString(expected2Bytes), Arrays.toString(actual2Bytes));

      final byte[] expected3Bytes = {0x34, 0x56, 0x78};
      final byte[] actual3Bytes = ByteSerializableStrategy.toBigEndianBytes(input, 3);
      assertEquals(Arrays.toString(expected3Bytes), Arrays.toString(actual3Bytes));

      final byte[] expected4Bytes = {0x12, 0x34, 0x56, 0x78};
      final byte[] actual4Bytes = ByteSerializableStrategy.toBigEndianBytes(input, 4);
      assertEquals(Arrays.toString(expected4Bytes), Arrays.toString(actual4Bytes));

      final byte[] expected5Bytes = {0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual5Bytes = ByteSerializableStrategy.toBigEndianBytes(input, 5);
      assertEquals(Arrays.toString(expected5Bytes), Arrays.toString(actual5Bytes));

      final byte[] expected6Bytes = {0x56, 0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual6Bytes = ByteSerializableStrategy.toBigEndianBytes(input, 6);
      assertEquals(Arrays.toString(expected6Bytes), Arrays.toString(actual6Bytes));

      final byte[] expected7Bytes = {0x34, 0x56, 0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual7Bytes = ByteSerializableStrategy.toBigEndianBytes(input, 7);
      assertEquals(Arrays.toString(expected7Bytes), Arrays.toString(actual7Bytes));

      final byte[] expected8Bytes = {0x12, 0x34, 0x56, 0x78, 0x12, 0x34, 0x56, 0x78};
      final byte[] actual8Bytes = ByteSerializableStrategy.toBigEndianBytes(input, 8);
      assertEquals(Arrays.toString(expected8Bytes), Arrays.toString(actual8Bytes));

      //technically if you asked for more it would 0 pad but that's undefined behavior
   }
}
