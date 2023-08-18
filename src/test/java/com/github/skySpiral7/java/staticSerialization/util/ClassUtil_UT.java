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
      assertFalse(ClassUtil.isPrimitiveOrBox(String.class));

      assertTrue(ClassUtil.isPrimitiveOrBox(Byte.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(Short.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(Integer.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(Long.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(Float.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(Double.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(Boolean.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(Character.class));

      assertTrue(ClassUtil.isPrimitiveOrBox(byte.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(short.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(int.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(long.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(float.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(double.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(boolean.class));
      assertTrue(ClassUtil.isPrimitiveOrBox(char.class));

      assertFalse(ClassUtil.isPrimitiveOrBox(Void.class));
      assertFalse(ClassUtil.isPrimitiveOrBox(void.class));
   }

   @Test
   public void unboxClass()
   {
      assertEquals(byte.class, ClassUtil.unboxClass(Byte.class));
      assertEquals(short.class, ClassUtil.unboxClass(Short.class));
      assertEquals(int.class, ClassUtil.unboxClass(Integer.class));
      assertEquals(long.class, ClassUtil.unboxClass(Long.class));
      assertEquals(float.class, ClassUtil.unboxClass(Float.class));
      assertEquals(double.class, ClassUtil.unboxClass(Double.class));
      assertEquals(boolean.class, ClassUtil.unboxClass(Boolean.class));
      assertEquals(char.class, ClassUtil.unboxClass(Character.class));
   }

   @Test
   public void unboxClass_throws_givenString()
   {
      try
      {
         ClassUtil.unboxClass(String.class);
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
         ClassUtil.unboxClass(Void.class);
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
      assertEquals(Byte.class, ClassUtil.boxClass(byte.class));
      assertEquals(Short.class, ClassUtil.boxClass(short.class));
      assertEquals(Integer.class, ClassUtil.boxClass(int.class));
      assertEquals(Long.class, ClassUtil.boxClass(long.class));
      assertEquals(Float.class, ClassUtil.boxClass(float.class));
      assertEquals(Double.class, ClassUtil.boxClass(double.class));
      assertEquals(Boolean.class, ClassUtil.boxClass(boolean.class));
      assertEquals(Character.class, ClassUtil.boxClass(char.class));
   }

   @Test
   public void boxClass_throws_givenString()
   {
      try
      {
         ClassUtil.boxClass(String.class);
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
         ClassUtil.boxClass(void.class);
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("void has no box class", actual.getMessage());
      }
   }
}
