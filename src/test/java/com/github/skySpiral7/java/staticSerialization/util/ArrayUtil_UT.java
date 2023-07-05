package com.github.skySpiral7.java.staticSerialization.util;

import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

public class ArrayUtil_UT
{
   @Test
   public void countArrayDimensions()
   {
      assertThat(ArrayUtil.countArrayDimensions(byte[].class), is(1));
      assertThat(ArrayUtil.countArrayDimensions(Byte[][][].class), is(3));
      assertThat(ArrayUtil.countArrayDimensions(Byte.class), is(0));
   }

   @Test
   public void getBaseComponentType()
   {
      assertThat(ArrayUtil.getBaseComponentType(byte[].class), is((Object) byte.class));
      assertThat(ArrayUtil.getBaseComponentType(Byte[][][].class), is((Object) Byte.class));
      assertThat(ArrayUtil.getBaseComponentType(Byte.class), is(nullValue()));
   }
}
