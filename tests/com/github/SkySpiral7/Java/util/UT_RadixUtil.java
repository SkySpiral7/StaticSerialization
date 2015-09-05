package com.github.SkySpiral7.Java.util;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class UT_RadixUtil {

    @Test
    public void toString_long_int() {
   	 assertEquals("11111", RadixUtil.toString(5, 1));
   	 assertEquals("-1111111111", RadixUtil.toString(-10, 1));
   	 assertEquals("", RadixUtil.toString(0, 1));

   	 assertEquals("0", RadixUtil.toString(0, 40));
   	 assertEquals("-10", RadixUtil.toString(-2, 2));
   	 assertEquals("-8000000000000000", RadixUtil.toString(Long.MIN_VALUE, 16));

   	 assertEquals("Z", RadixUtil.toString(61, 62));
   	 assertEquals("10", RadixUtil.toString(62, 62));
   	 assertEquals("100", RadixUtil.toString(3844, 62));
   	 assertEquals("Zy", RadixUtil.toString(3816, 62));
    }

}
