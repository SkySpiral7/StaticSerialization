package com.github.skySpiral7.java.staticSerialization.util;

import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

public class ArrayUtil_UT
{
   private ArrayUtil testObject = new ArrayUtil();

   @Test
   public void countArrayDimensions()
   {
      assertThat(testObject.countArrayDimensions(byte[].class), is(1));
      assertThat(testObject.countArrayDimensions(Byte[][][].class), is(3));
      assertThat(testObject.countArrayDimensions(Byte.class), is(0));
   }

   @Test
   public void getBaseComponentType()
   {
      assertThat(testObject.getBaseComponentType(byte[].class), is((Object) byte.class));
      assertThat(testObject.getBaseComponentType(Byte[][][].class), is((Object) Byte.class));
      assertThat(testObject.getBaseComponentType(Byte.class), is(nullValue()));
   }
}
