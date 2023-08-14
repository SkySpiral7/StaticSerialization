package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class ObjectReaderRegistry_UT
{
   private ObjectReaderRegistry testObject;

   @BeforeEach
   public void setUp()
   {
      testObject = new ObjectReaderRegistry();
   }

   @Test
   public void isRegistered_returnsFalse_givenUnregisteredObject()
   {
      assertFalse(testObject.isRegistered(new Object()));
   }

   @Test
   public void isRegistered_returnsTrue_givenRegisteredObject()
   {
      final Object data = new Object();
      testObject.reserveIdForLater();
      testObject.registerObject(data);
      assertTrue(testObject.isRegistered(data));
   }

   @Test
   public void isRegistered_throwsNpe_givenNull()
   {
      assertThrows(NullPointerException.class, () -> testObject.isRegistered(null));
   }

   @Test
   public void registerObject()
   {
      final Object data = new Object();
      testObject.reserveIdForLater();
      testObject.registerObject(data);
      assertTrue(testObject.isRegistered(data));
   }

   @Test
   public void registerObject_throwsNpe_givenNull()
   {
      assertThrows(NullPointerException.class, () -> testObject.registerObject(null));
   }

   @Test
   public void registerObject_doesNothing_givenRegisteredObject()
   {
      final Object data = new Object();
      testObject.reserveIdForLater();
      testObject.registerObject(data);
      testObject.registerObject(data);
   }

   @Test
   public void registerObject_idsAreLifo_givenMultipleObjects()
   {
      final Object data0 = "0";
      final Object data1 = "1";
      testObject.reserveIdForLater();
      testObject.reserveIdForLater();
      testObject.registerObject(data1);
      testObject.registerObject(data0);
      assertEquals(data0, testObject.getRegisteredObject(0));
      assertEquals(data1, testObject.getRegisteredObject(1));
   }

   @Test
   public void registerObject_throws_whenNoIdReserved()
   {
      final Object data = new Object();

      try
      {
         testObject.registerObject(data);
         fail("Should've thrown");
      }
      catch (IllegalStateException actual)
      {
         final String expectedMessage = "id not found. Make sure registerObject is only called for the "
               + "root object and that ObjectStreamReader.readObject etc are used as an "
               + "entry point for reading the stream.";
         assertEquals(expectedMessage, actual.getMessage());
      }
   }

   @Test
   public void getRegisteredObject()
   {
      final Object data = new Object();
      testObject.reserveIdForLater();
      testObject.registerObject(data);
      assertEquals(data, testObject.getRegisteredObject(0));
   }

   @Test
   public void getRegisteredObject_throws_whenIdNegative()
   {
      try
      {
         testObject.getRegisteredObject(-2);
         fail("Should've thrown");
      }
      catch (StreamCorruptedException actual)
      {
         assertEquals("invalid id. registry.size=0 but found id=-2", actual.getMessage());
      }
   }

   @Test
   public void getRegisteredObject_throws_whenIdNotFound()
   {
      final Object data = new Object();
      testObject.reserveIdForLater();
      testObject.registerObject(data);
      try
      {
         testObject.getRegisteredObject(10);
         fail("Should've thrown");
      }
      catch (StreamCorruptedException actual)
      {
         assertEquals("invalid id. registry.size=1 but found id=10", actual.getMessage());
      }
   }
}
