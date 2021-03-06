package com.github.skySpiral7.java.staticSerialization;

import java.io.File;
import java.math.RoundingMode;

import com.github.skySpiral7.java.staticSerialization.exception.NoMoreDataException;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ObjectStreamReader_UT
{
   @Test
   public void readBytes_throw() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readBytes_throw.", ".txt");
      tempFile.deleteOnExit();
      final byte[] fileContents = {'!', 0x0a};
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.hasData());
      try
      {
         testObject.readObject(Short.class);
         fail("Didn't throw");
      }
      catch (final NoMoreDataException actual)
      {
         assertEquals("expected 2 bytes, found 1 bytes", actual.getMessage());
         //this indirectly tests hasData(int) and remainingBytes(). hasData() is tested everywhere
      }

      testObject.close();
   }

   @Test
   public void readObject_allowsCast_givenNoArg() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObject_allowsCast_givenNoArg.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = {'~', 3};  //~ is byte
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertEquals(Byte.valueOf((byte) 3), testObject.readObject());
      testObject.close();
   }

   @Test
   public void readObjectStrictly_happyPath() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readObjectStrictly_happyPath.", ".txt");
      tempFile.deleteOnExit();

      final byte[] fileContents = {'+', '-',  //+ is true, - is false
            '~', 3};  //~ is byte
      FileIoUtil.writeToFile(tempFile, fileContents);

      FileIoUtil.appendToFile(tempFile, "java.math.RoundingMode;");
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 1});

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertTrue(testObject.readObjectStrictly(Boolean.class));
      assertFalse(testObject.readObjectStrictly(Boolean.class));
      assertEquals(Byte.valueOf((byte) 3), testObject.readObjectStrictly(Byte.class));
      assertEquals(RoundingMode.DOWN, testObject.readObjectStrictly(RoundingMode.class));
      testObject.close();
   }

   public static final class ReflectiveClass implements StaticSerializable
   {
      private int field = 0xdead_beef;

      public static ReflectiveClass readFromStream(final ObjectStreamReader reader)
      {
         final ReflectiveClass result = new ReflectiveClass();
         reader.readFieldsReflectively(result);
         return result;
      }

      @Override
      public void writeToStream(final ObjectStreamWriter writer){}
   }

   @Test
   public void readFieldsReflectively() throws Exception
   {
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readFieldsReflectively.", ".txt");
      tempFile.deleteOnExit();
      final String header = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$ReflectiveClass;@";
      FileIoUtil.writeToFile(tempFile, header);
      final byte[] fileContents = {0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe};
      FileIoUtil.appendToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      assertEquals(0x0afe_babeL, testObject.readObject(ReflectiveClass.class).field);

      testObject.close();
   }

}
