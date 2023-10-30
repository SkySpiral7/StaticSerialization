package com.github.skySpiral7.java.staticSerialization.util;

import org.junit.jupiter.api.Test;

import java.math.RoundingMode;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class ClassUtil_UT
{
   private final ClassUtil testObject = new ClassUtil();

   @Test
   public <T extends Enum<T>> void cast_compiles()
   {
      final Class<?> questionable = RoundingMode.class;

      @SuppressWarnings("unchecked") final Class<T> unchecked = ((Class<T>) questionable);
      //not convenient and only possible because of the method's capturing

      assertThat(Enum.valueOf(unchecked, "UP"), is(RoundingMode.UP));
      assertThat(Enum.valueOf(ClassUtil.cast(questionable), "UP"), is(RoundingMode.UP));
   }

   @Test
   public void isBoxedPrimitive()
   {
      assertFalse(testObject.isPrimitiveOrBox(String.class));

      assertTrue(testObject.isPrimitiveOrBox(Byte.class));
      assertTrue(testObject.isPrimitiveOrBox(Short.class));
      assertTrue(testObject.isPrimitiveOrBox(Integer.class));
      assertTrue(testObject.isPrimitiveOrBox(Long.class));
      assertTrue(testObject.isPrimitiveOrBox(Float.class));
      assertTrue(testObject.isPrimitiveOrBox(Double.class));
      assertTrue(testObject.isPrimitiveOrBox(Boolean.class));
      assertTrue(testObject.isPrimitiveOrBox(Character.class));

      assertTrue(testObject.isPrimitiveOrBox(byte.class));
      assertTrue(testObject.isPrimitiveOrBox(short.class));
      assertTrue(testObject.isPrimitiveOrBox(int.class));
      assertTrue(testObject.isPrimitiveOrBox(long.class));
      assertTrue(testObject.isPrimitiveOrBox(float.class));
      assertTrue(testObject.isPrimitiveOrBox(double.class));
      assertTrue(testObject.isPrimitiveOrBox(boolean.class));
      assertTrue(testObject.isPrimitiveOrBox(char.class));

      assertFalse(testObject.isPrimitiveOrBox(Void.class));
      assertFalse(testObject.isPrimitiveOrBox(void.class));
   }

   @Test
   public void unboxClass()
   {
      assertEquals(byte.class, testObject.unboxClass(Byte.class));
      assertEquals(short.class, testObject.unboxClass(Short.class));
      assertEquals(int.class, testObject.unboxClass(Integer.class));
      assertEquals(long.class, testObject.unboxClass(Long.class));
      assertEquals(float.class, testObject.unboxClass(Float.class));
      assertEquals(double.class, testObject.unboxClass(Double.class));
      assertEquals(boolean.class, testObject.unboxClass(Boolean.class));
      assertEquals(char.class, testObject.unboxClass(Character.class));
   }

   @Test
   public void unboxClass_throws_givenString()
   {
      try
      {
         testObject.unboxClass(String.class);
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("java.lang.String isn't a box class", actual.getMessage());
      }
   }

   @Test
   public void unboxClass_throws_givenVoid()
   {
      try
      {
         testObject.unboxClass(Void.class);
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("java.lang.Void isn't a box class", actual.getMessage());
      }
   }

   @Test
   public void boxClass()
   {
      assertEquals(Byte.class, testObject.boxClass(byte.class));
      assertEquals(Short.class, testObject.boxClass(short.class));
      assertEquals(Integer.class, testObject.boxClass(int.class));
      assertEquals(Long.class, testObject.boxClass(long.class));
      assertEquals(Float.class, testObject.boxClass(float.class));
      assertEquals(Double.class, testObject.boxClass(double.class));
      assertEquals(Boolean.class, testObject.boxClass(boolean.class));
      assertEquals(Character.class, testObject.boxClass(char.class));
   }

   @Test
   public void boxClass_throws_givenString()
   {
      try
      {
         testObject.boxClass(String.class);
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("java.lang.String isn't a primitive class", actual.getMessage());
      }
   }

   @Test
   public void boxClass_throws_givenVoid()
   {
      try
      {
         testObject.boxClass(void.class);
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("void has no box class", actual.getMessage());
      }
   }
}
