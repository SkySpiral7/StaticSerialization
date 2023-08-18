package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ObjectWriterRegistry_UT
{
   private ObjectWriterRegistry testObject;

   @BeforeEach
   public void setUp()
   {
      testObject = new ObjectWriterRegistry();
   }

   @Test
   public void registerObject_throwsNpe_givenNull()
   {
      assertThrows(NullPointerException.class, () -> testObject.registerObject(null));
   }

   @Test
   public void registerObject_doesNothing_givenRegisteredObject()
   {
      final Object data = "5";
      testObject.registerObject(data);
      assertEquals(0L, testObject.getId(data).longValue());
      testObject.registerObject(data);
      assertEquals(0L, testObject.getId(data).longValue());
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
      assertEquals(0L, actual.longValue());
   }

   @Test
   public void getId_throwsNpe_givenNull()
   {
      assertThrows(NullPointerException.class, () -> testObject.getId(null));
   }
}
