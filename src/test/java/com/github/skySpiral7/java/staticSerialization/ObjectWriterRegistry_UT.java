package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ObjectWriterRegistry_UT
{
   private ObjectWriterRegistry testObject;

   @Before
   public void setUp()
   {
      testObject = new ObjectWriterRegistry();
   }

   @Test(expected = NullPointerException.class)
   public void registerObject_throwsNpe_givenNull()
   {
      testObject.registerObject(null);
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

   @Test(expected = NullPointerException.class)
   public void getId_throwsNpe_givenNull()
   {
      testObject.getId(null);
   }
}
