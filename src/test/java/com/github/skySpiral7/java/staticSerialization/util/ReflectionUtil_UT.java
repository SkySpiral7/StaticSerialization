package com.github.skySpiral7.java.staticSerialization.util;

import com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class ReflectionUtil_UT
{
   private final ReflectionUtil testObject = new ReflectionUtil();

   private static class Class_getAllSerializableFields
   {
      public final int fieldFinal = 1;
      public transient int fieldTransient = 1;
      public static int fieldStatic = 1;
      public int fieldPrimitive = 1;
      public Integer fieldBoxedPrimitive = 1;
      public String fieldString = "1";
      public SimpleHappy fieldStaticSerializable = new SimpleHappy(1);
      public BigInteger fieldSerializable = BigInteger.ONE;
      public int[] fieldArray = {1};
      public Object fieldOther = 1;
   }

   @Test
   public void getAllSerializableFields_excludesBasedOnModifier() throws Exception
   {
      final List<Field> expected = new ArrayList<>();
      //sorted by field name
      expected.add(Class_getAllSerializableFields.class.getField("fieldArray"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldBoxedPrimitive"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldOther"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldPrimitive"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldSerializable"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldStaticSerializable"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldString"));

      final List<Field> actual = testObject.getAllSerializableFields(Class_getAllSerializableFields.class);

      assertThat(actual, is(expected));
      assertFalse(actual.contains(Class_getAllSerializableFields.class.getField("fieldFinal")));
      assertFalse(actual.contains(Class_getAllSerializableFields.class.getField("fieldTransient")));
      assertFalse(actual.contains(Class_getAllSerializableFields.class.getField("fieldStatic")));
   }

   @Test
   public void getAllSerializableFields_includesInheritedFields() throws Exception
   {
      class ClassA
      {
         public int field$Z = 1;
      }
      class ClassB extends ClassA
      {
         public int fieldC = 2;
      }
      //by virtue of being non-static these local classes contain a generated field (this$0) which references ReflectionUtil_UT
      //this$0 is final and thus excluded

      final List<Field> expected = new ArrayList<>();
      //sorted by class name
      expected.add(ClassA.class.getField("field$Z"));
      expected.add(ClassB.class.getField("fieldC"));

      assertThat(testObject.getAllSerializableFields(ClassB.class), is(expected));
   }
}
