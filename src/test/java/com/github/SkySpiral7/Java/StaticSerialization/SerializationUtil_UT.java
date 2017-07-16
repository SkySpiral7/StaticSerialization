package com.github.SkySpiral7.Java.StaticSerialization;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import com.github.SkySpiral7.Java.StaticSerialization.testClasses.SimpleHappy;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class SerializationUtil_UT
{

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
      public Object fieldOther = 1;
   }

   @Test
   public void getAllSerializableFields() throws Exception
   {
      final List<Field> expected = new ArrayList<>();
      expected.add(Class_getAllSerializableFields.class.getField("fieldPrimitive"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldBoxedPrimitive"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldString"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldStaticSerializable"));
      expected.add(Class_getAllSerializableFields.class.getField("fieldSerializable"));

      final List<Field> actual = SerializationUtil.getAllSerializableFields(Class_getAllSerializableFields.class);

      assertThat(actual, is(expected));
   }

}
