package com.github.SkySpiral7.Java.serialization;

import java.lang.reflect.Field;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

import com.github.SkySpiral7.Java.util.ClassUtil;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class UT_ClassUtil
{
   @Test
   public <T extends Enum<T>> void cast_compiles()
   {
      final Class<?> questionable = RoundingMode.class;

      @SuppressWarnings("unchecked")
      final Class<T> unchecked = ((Class<T>) questionable);
      //not convenient and only possible because of the method's capturing

      assertThat(Enum.valueOf(unchecked, "UP"), is(RoundingMode.UP));
      assertThat(Enum.valueOf(ClassUtil.cast(questionable), "UP"), is(RoundingMode.UP));
   }

   @Test
   public void getAllFields() throws Exception
   {
      class ClassA
      {
         @SuppressWarnings("unused")
         public int fieldA = 1;
      }
      class ClassB extends ClassA
      {
         @SuppressWarnings("unused")
         public int fieldB = 2;
      }
      //by virtue of being non-static these local classes contain a generated field (this$0) which references UT_ClassUtil

      final List<Field> expected = new ArrayList<>();
      expected.add(ClassB.class.getField("fieldB"));
      expected.add(ClassA.class.getField("fieldA"));

      assertThat(ClassUtil.getAllFields(ClassB.class), is(expected));
   }

   @Test
   public void isBoxedPrimitive()
   {
      assertFalse(ClassUtil.isBoxedPrimitive(String.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Byte.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Short.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Integer.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Long.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Float.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Double.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Boolean.class));
      assertTrue(ClassUtil.isBoxedPrimitive(Character.class));
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
         assertEquals("java.lang.String isn't a primitive class or is void.class", actual.getMessage());
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
         assertEquals("void isn't a primitive class or is void.class", actual.getMessage());
      }
   }

}
