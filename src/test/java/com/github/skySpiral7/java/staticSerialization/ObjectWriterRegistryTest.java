package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class ObjectWriterRegistryTest
{
   private ObjectWriterRegistry testObject;
   private final ClassUtil classUtil = new ClassUtil();

   @BeforeEach
   public void setUp()
   {
      testObject = new ObjectWriterRegistry(classUtil);
   }

   @Test
   public void registerObject_doesNothing_givenNull()
   {
      testObject.registerObject(null);
      assertNull(testObject.getId(null));
   }

   @Test
   public void registerObject_doesNothing_givenRegisteredObject()
   {
      final Object data = "5";
      testObject.registerObject(data);
      assertEquals(0, testObject.getId(data));
      testObject.registerObject(data);
      assertEquals(0, testObject.getId(data));
   }

   @Test
   public void registerObject_doesNothing_givenSmallPrimitive()
   {
      final Object data = 5;
      testObject.registerObject(data);
      assertNull(testObject.getId(data));
   }

   @Test
   public void registerObject_registers_givenLong()
   {
      final Long data = 5L;
      testObject.registerObject(data);
      assertEquals(0, testObject.getId(data));
      testObject.registerObject(data);
      assertEquals(0, testObject.getId(data));
   }

   @Test
   public void registerObject_registers_givenDouble()
   {
      final Double data = 5d;
      testObject.registerObject(data);
      assertEquals(0, testObject.getId(data));
      testObject.registerObject(data);
      assertEquals(0, testObject.getId(data));
   }

   @Test
   public void getId_returnsNull_givenUnregisteredObject()
   {
      assertNull(testObject.getId(5));
   }

   @Test
   public void getId_returnsId_givenRegisteredObject()
   {
      final Object data = "5";
      testObject.registerObject(data);
      final Integer actual = testObject.getId(data);
      assertEquals(0, actual);
   }

   @Test
   public void getId_returnsNull_givenNull()
   {
      assertNull(testObject.getId(null));
   }
}
