package com.github.SkySpiral7.Java.util;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class RadixUtil_UT
{

   @Test
   public void toString_long_int()
   {
      assertEquals("11111", RadixUtil.toString(5, 1));
      assertEquals("-1111111111", RadixUtil.toString(-10, 1));
      assertEquals("", RadixUtil.toString(0, 1));

      assertEquals("0", RadixUtil.toString(0, 40));
      assertEquals("-10", RadixUtil.toString(-2, 2));
      assertEquals("-8000000000000000", RadixUtil.toString(Long.MIN_VALUE, 16));

      assertEquals("-Z", RadixUtil.toString(-61, 62));
      assertEquals("10", RadixUtil.toString(62, 62));
      assertEquals("100", RadixUtil.toString(3844, 62));
      assertEquals("Zy", RadixUtil.toString(3816, 62));
   }

   @Test
   public void parseLong_String_int()
   {
      assertEquals(5, RadixUtil.parseLong("+11111", 1));
      assertEquals(-10, RadixUtil.parseLong("-1111111111", 1));
      assertEquals(0, RadixUtil.parseLong("", 1));
      assertEquals(0, RadixUtil.parseLong("-", 1));

      assertEquals(0, RadixUtil.parseLong("0", 40));
      assertEquals(-2, RadixUtil.parseLong("-10", 2));
      assertEquals(Long.MIN_VALUE, RadixUtil.parseLong("-8000000000000000", 16));

      assertEquals(-61, RadixUtil.parseLong("-Z", 62));
      assertEquals(62, RadixUtil.parseLong("10", 62));
      assertEquals(3844, RadixUtil.parseLong("100", 62));
      assertEquals(3816, RadixUtil.parseLong("Zy", 62));
   }

   @Test
   public void getDigitValue()
   {
      assertEquals(-1, RadixUtil.getDigitValue('0', 1));
      assertEquals(1, RadixUtil.getDigitValue('1', 1));
      assertEquals(-1, RadixUtil.getDigitValue('2', 1));
      assertEquals(0, RadixUtil.getDigitValue('0', 2));
      assertEquals(1, RadixUtil.getDigitValue('1', 2));
      assertEquals(-1, RadixUtil.getDigitValue('2', 2));

      assertEquals(0, RadixUtil.getDigitValue('0', 35));
      assertEquals(1, RadixUtil.getDigitValue('1', 60));
      assertEquals(-1, RadixUtil.getDigitValue('+', 62));

      assertEquals(-1, RadixUtil.getDigitValue('Z', 36));
      assertEquals(-1, RadixUtil.getDigitValue('Z', 10));
      assertEquals(61, RadixUtil.getDigitValue('Z', 62));
   }

}
