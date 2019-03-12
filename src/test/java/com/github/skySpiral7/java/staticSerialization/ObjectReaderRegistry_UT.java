package com.github.skySpiral7.java.staticSerialization;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;

public class ObjectReaderRegistry_UT
{
//   private ObjectReaderRegistry testObject;
//
//   @Before
//   public void setUp()
//   {
//      testObject = new ObjectReaderRegistry();
//   }
//
//   @Test(expected = NullPointerException.class)
//   public void registerObject_throwsNpe_givenNullId()
//   {
//      testObject.registerObject(null, "");
//   }
//
//   @Test(expected = NullPointerException.class)
//   public void registerObject_throwsNpe_givenNullValue()
//   {
//      testObject.registerObject("", null);
//   }
//
//   @Test(expected = NullPointerException.class)
//   public void getRegisteredObject_throwsNpe_givenNullId()
//   {
//      testObject.getRegisteredObject(null);
//   }
//
//   @Test
//   public void getRegisteredObject_returnsSame_whenIdFound()
//   {
//      final String id = UUID.randomUUID().toString();
//      final Object expected = new Object();
//
//      testObject.registerObject(id, expected);
//      assertSame(expected, testObject.getRegisteredObject(id));
//   }
//
//   @Test
//   public void getRegisteredObject_returnsNull_whenIdNotFound()
//   {
//      testObject.registerObject(UUID.randomUUID().toString(), "test me");
//      assertNull(testObject.getRegisteredObject(UUID.randomUUID().toString()));
//   }
//
//   @Test(expected = NullPointerException.class)
//   public void readObjectOrId_throwsNpe_givenNullReader()
//   {
//      testObject.readObjectOrId(null);
//   }
//
//   @Test
//   public void readObjectOrId_returnsSame_whenIdFound() throws IOException
//   {
//      final String id = UUID.randomUUID().toString();
//      final File tempFile = writeIdToFile("ObjectReaderRegistry_UT.TempFile.readObjectOrId_returnsSame_whenIdFound.", id);
//
//      final Object data = new Object();
//      testObject.registerObject(id, data);
//
//      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
//      assertSame(data, testObject.readObjectOrId(reader));
//
//      reader.close();
//   }
//
//   @Test
//   public void readObjectOrId_returnsNull_whenIdNotFound() throws IOException
//   {
//      final File tempFile = writeIdToFile("ObjectReaderRegistry_UT.TempFile.readObjectOrId_returnsNull_whenIdNotFound.",
//            UUID.randomUUID().toString());
//
//      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
//      assertNull(testObject.readObjectOrId(reader));
//
//      reader.close();
//   }
//
//   @Test
//   public void readObjectOrId_throws_whenUnclaimedIdAlreadyExists() throws IOException
//   {
//      final File tempFile = writeIdToFile("ObjectReaderRegistry_UT.TempFile.readObjectOrId_throws_whenUnclaimedIdAlreadyExists.",
//            UUID.randomUUID().toString());
//      FileIoUtil.appendToFile(tempFile, "*");
//      final byte[] idSize = {0, 0, 0, 36};
//      FileIoUtil.appendToFile(tempFile, idSize);
//      FileIoUtil.appendToFile(tempFile, UUID.randomUUID().toString());
//
//      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
//      testObject.readObjectOrId(reader);
//      try
//      {
//         testObject.readObjectOrId(reader);
//         fail("Should've thrown");
//      }
//      catch (IllegalStateException actual)
//      {
//         assertEquals("Failed to call claimId. Stopping gracefully.", actual.getMessage());
//      }
//
//      reader.close();
//   }
//
//   @Test(expected = NullPointerException.class)
//   public void claimId_throwsNpe_givenNullInput()
//   {
//      testObject.claimId(null);
//   }
//
//   @Test
//   public void claimId_registersObject_whenIdExists() throws IOException
//   {
//      final String id = UUID.randomUUID().toString();
//      final File tempFile = writeIdToFile("ObjectReaderRegistry_UT.TempFile.claimId_registersObject_whenIdExists.", id);
//
//      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
//      testObject.readObjectOrId(reader);
//      reader.close();
//
//      final Object data = new Object();
//      testObject.claimId(data);
//      assertSame(data, testObject.getRegisteredObject(id));
//   }
//
//   @Test(expected = NullPointerException.class)
//   public void claimId_throwsNpe_whenNothingToClaim() throws IOException
//   {
//      testObject.claimId(new Object());
//   }
//
//   private File writeIdToFile(final String prefix, final String id) throws IOException
//   {
//      final File tempFile = File.createTempFile(prefix, ".txt");
//      tempFile.deleteOnExit();
//      FileIoUtil.writeToFile(tempFile, "*");
//      final byte[] idSize = {0, 0, 0, (byte) id.length()};
//      FileIoUtil.appendToFile(tempFile, idSize);
//      FileIoUtil.appendToFile(tempFile, id);
//      return tempFile;
//   }

}
