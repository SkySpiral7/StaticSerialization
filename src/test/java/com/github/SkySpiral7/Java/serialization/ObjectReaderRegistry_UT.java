package com.github.SkySpiral7.Java.serialization;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

import com.github.SkySpiral7.Java.util.FileIoUtil;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

public class ObjectReaderRegistry_UT
{
   private ObjectReaderRegistry testObject;

   @Before
   public void setUp()
   {
      testObject = new ObjectReaderRegistry();
   }

   @Test(expected = NullPointerException.class)
   public void registerObject_throwsNpe_givenNullId()
   {
      testObject.registerObject(null, "");
   }

   @Test(expected = NullPointerException.class)
   public void registerObject_throwsNpe_givenNullValue()
   {
      testObject.registerObject("", null);
   }

   @Test(expected = NullPointerException.class)
   public void getRegisteredObject_throwsNpe_givenNullId()
   {
      testObject.getRegisteredObject(null);
   }

   @Test
   public void getRegisteredObject_returnsSame_whenIdFound()
   {
      final String id = UUID.randomUUID().toString();
      final Object expected = new Object();

      testObject.registerObject(id, expected);
      assertSame(expected, testObject.getRegisteredObject(id));
   }

   @Test
   public void getRegisteredObject_returnsNull_whenIdNotFound()
   {
      testObject.registerObject(UUID.randomUUID().toString(), "test me");
      assertNull(testObject.getRegisteredObject(UUID.randomUUID().toString()));
   }

   @Test(expected = NullPointerException.class)
   public void readObjectOrId_throwsNpe_givenNullReader()
   {
      testObject.readObjectOrId(null);
   }

   @Test
   public void readObjectOrId_returnsSame_whenIdFound() throws IOException
   {
      final String id = UUID.randomUUID().toString();
      final File tempFile = writeIdToFile("ObjectReaderRegistry_UT.TempFile.readObjectOrId_returnsSame_whenIdFound.", id);

      final Object data = new Object();
      testObject.registerObject(id, data);

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertSame(data, testObject.readObjectOrId(reader));

      reader.close();
   }

   @Test
   public void readObjectOrId_returnsNull_whenIdNotFound() throws IOException
   {
      final File tempFile = writeIdToFile("ObjectReaderRegistry_UT.TempFile.readObjectOrId_returnsNull_whenIdNotFound.",
            UUID.randomUUID().toString());

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      assertNull(testObject.readObjectOrId(reader));

      reader.close();
   }

   @Test(expected = NullPointerException.class)
   public void claimId_throwsNpe_givenNullInput()
   {
      testObject.claimId(null);
   }

   @Test
   public void claimId_registersObject_whenIdExists() throws IOException
   {
      final String id = UUID.randomUUID().toString();
      final File tempFile = writeIdToFile("ObjectReaderRegistry_UT.TempFile.claimId_registersObject_whenIdExists.", id);

      final ObjectStreamReader reader = new ObjectStreamReader(tempFile);
      testObject.readObjectOrId(reader);
      reader.close();

      final Object data = new Object();
      testObject.claimId(data);
      assertSame(data, testObject.getRegisteredObject(id));
   }

   @Test(expected = NullPointerException.class)
   public void claimId_throwsNpe_whenNothingToClaim() throws IOException
   {
      testObject.claimId(new Object());
   }

   private File writeIdToFile(final String prefix, final String id) throws IOException
   {
      final File tempFile = File.createTempFile(prefix, ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.String|".getBytes(StandardCharsets.UTF_8), false);
      final byte[] idSize = new byte[]{(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) id.length()};
      FileIoUtil.writeToFile(tempFile, idSize, true);
      FileIoUtil.writeToFile(tempFile, id.getBytes(StandardCharsets.UTF_8), true);
      return tempFile;
   }

}
