package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.util.ClassUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class ObjectReaderRegistryTest
{
   private ObjectReaderRegistry testObject;
   private final ClassUtil classUtil = new ClassUtil();

   @BeforeEach
   public void setUp()
   {
      testObject = new ObjectReaderRegistry(classUtil);
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
      testObject.reserveIdForLater(Object.class);
      testObject.registerObject(data);
      assertTrue(testObject.isRegistered(data));
   }

   @Test
   public void isRegistered_returnsFalse_givenNull()
   {
      assertFalse(testObject.isRegistered(null));
   }

   @Test
   public void registerObject()
   {
      final Object data = new Object();
      testObject.reserveIdForLater(Object.class);
      testObject.registerObject(data);
      assertTrue(testObject.isRegistered(data));
   }

   @Test
   public void registerObject_doesNothing_givenNull()
   {
      testObject.registerObject(null);
      assertFalse(testObject.isRegistered(null));
   }

   @Test
   public void reserveIdForLater_doesNothing_givenSmallPrimitive()
   {
      final Object data = 5;
      testObject.reserveIdForLater(Integer.class);
      testObject.registerObject(data);
      assertFalse(testObject.isRegistered(data));
   }

   @Test
   public void reserveIdForLater_reserves_givenLong()
   {
      final Object data = 5L;
      testObject.reserveIdForLater(Long.class);
      testObject.registerObject(data);
      assertEquals(data, testObject.getRegisteredObject(0));
   }

   @Test
   public void reserveIdForLater_reserves_givenDouble()
   {
      final Object data = 5d;
      testObject.reserveIdForLater(Double.class);
      testObject.registerObject(data);
      assertEquals(data, testObject.getRegisteredObject(0));
   }

   @Test
   public void registerObject_doesNothing_givenRegisteredObject()
   {
      final Object data = new Object();
      testObject.reserveIdForLater(Object.class);
      testObject.registerObject(data);
      testObject.registerObject(data);
   }

   @Test
   public void registerObject_idsAreLifo_givenMultipleObjects()
   {
      final Object data0 = "0";
      final Object data1 = "1";
      testObject.reserveIdForLater(Object.class);
      testObject.reserveIdForLater(Object.class);
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
      testObject.reserveIdForLater(Object.class);
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
      testObject.reserveIdForLater(Object.class);
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
