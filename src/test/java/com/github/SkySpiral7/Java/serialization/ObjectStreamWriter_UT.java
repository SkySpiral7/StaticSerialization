package com.github.SkySpiral7.Java.serialization;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.SkySpiral7.Java.exception.NotSerializableException;
import com.github.SkySpiral7.Java.util.BitWiseUtil;
import com.github.SkySpiral7.Java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ObjectStreamWriter_UT
{
   @Test
   public void constructor_throws()
   {
      try
      {
         new ObjectStreamWriter(new File(".")).close();
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("It is not possible to write to a directory", actual.getMessage());
      }
   }

   @Test
   public void constructor_clears() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.constructor_clears.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "test");
      new ObjectStreamWriter(tempFile).close();
      assertEquals("", FileIoUtil.readTextFile(tempFile));
   }

   @Test
   public void writeObject_overHead() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_overHead.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject((byte) 0xab);
      testObject.close();
      //@formatter:off
		final byte[] expected = new byte[] {
				(byte)38, (byte)66,  //"&B"
				(byte)0xab  //the data
		};
		//@formatter:on
      //don't use bytesToString since that assumes the header has UTF-8 encoding
      assertEquals(Arrays.toString(expected), Arrays.toString(FileIoUtil.readBinaryFile(tempFile)));
   }

   @Test
   public void writeObject_overHead_null() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_overHead_null.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(null);
      testObject.close();
      final byte[] expected = new byte[] {(byte) '|'};
      //don't use bytesToString since that assumes the header has UTF-8 encoding
      assertEquals(Arrays.toString(expected), Arrays.toString(FileIoUtil.readBinaryFile(tempFile)));
   }

   @Test
   public void writeObject_stops_GenerateId() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_stops_GenerateId.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      @GenerateId
      class LocalWithGenerateId
      {
      }

      final String id = "f";
      final Object data = new LocalWithGenerateId();
      testObject.getObjectRegistry().registerObject(id, data);

      testObject.writeObject(data);
      testObject.close();
      final byte[] expected = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01,  //UTF-8 length (int)
            (byte) 0x66};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String overhead = "com.github.SkySpiral7.Java.serialization.ObjectStreamWriter_UT$1LocalWithGenerateId|&T";
      assertEquals(overhead, bytesToString(fileContents, expected.length));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, expected.length)));
   }

   @Test
   public void writeObject_continues_GenerateId() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_continues_GenerateId.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      @GenerateId
      class LocalWithGenerateIdAndWrite implements StaticSerializable
      {
         @Override
         public void writeToStream(final ObjectStreamWriter writer)
         {
            writer.writeObject((byte) 5);
         }
      }

      final Object data = new LocalWithGenerateIdAndWrite();
      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String overhead = "com.github.SkySpiral7.Java.serialization.ObjectStreamWriter_UT$1LocalWithGenerateIdAndWrite|&T";
      int offset = 0;
      assertEquals(overhead, bytesToString(subArrayWithLength(fileContents, offset, overhead.length()), 0));
      offset += overhead.length();
      assertEquals(Arrays.toString(new byte[] {0, 0, 0, 36}), Arrays.toString(subArrayWithLength(fileContents, offset, 4)));
      offset += 4;
      offset += 36;
      assertEquals("&B", bytesToString(subArrayWithLength(fileContents, offset, 2), 0));
      assertEquals("[5]", Arrays.toString(shortenBytes(fileContents, 1)));
   }

   @Test
   public void writeObject_byte() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_byte.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Byte data = (byte) 2;

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&B", bytesToString(fileContents, 1));
      assertEquals(2, fileContents[2]);
   }

   @Test
   public void writeObject_short() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_short.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Short data = (short) 0xcafe;
      final byte[] expected = {(byte) 0xca, (byte) 0xfe};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&S", bytesToString(fileContents, 2));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 2)));
   }

   @Test
   public void writeObject_int() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_int.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Integer data = 0xcafe_bead;
      final byte[] expected = {(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&I", bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   @Test
   public void writeObject_long() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_long.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Long data = 0xdead_beef__b100_d123L;
      final byte[] expected = {(byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, (byte) 0x00,
            (byte) 0xd1, (byte) 0x23};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&J", bytesToString(fileContents, 8));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 8)));
   }

   @Test
   public void writeObject_float() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_float.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Float data = Float.intBitsToFloat(0xcafe_bead);
      final byte[] expected = {(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&F", bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   @Test
   public void writeObject_double() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_double.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Double data = Double.longBitsToDouble(0xdead_beef__b100_d123L);
      final byte[] expected = {(byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, (byte) 0x00,
            (byte) 0xd1, (byte) 0x23};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&D", bytesToString(fileContents, 8));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 8)));
   }

   @Test
   public void writeObject_boolean() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_boolean.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(true);
      testObject.flush();
      byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&Z", bytesToString(fileContents, 1));
      assertEquals("[1]", Arrays.toString(shortenBytes(fileContents, 1)));

      FileIoUtil.writeToFile(tempFile, "");

      testObject.writeObject(false);
      testObject.close();
      fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&Z", bytesToString(fileContents, 1));
      assertEquals("[0]", Arrays.toString(shortenBytes(fileContents, 1)));
   }

   @Test
   public void writeObject_char() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_char.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject('f');
      testObject.flush();
      byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&C", bytesToString(fileContents, 2));
      assertEquals("[0, " + 0x66 + "]", Arrays.toString(shortenBytes(fileContents, 2)));

      FileIoUtil.writeToFile(tempFile, "");

      testObject.writeObject('\u221E');  //infinity sign is BMP non-private
      testObject.close();
      fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&C", bytesToString(fileContents, 2));
      assertEquals("[" + 0x22 + ", " + 0x1e + "]", Arrays.toString(shortenBytes(fileContents, 2)));
   }

   @Test
   public void writeObject_String() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_String.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject("f\u221E");  //infinity sign is BMP (3 UTF-8 bytes) non-private
      testObject.close();
      final byte[] expected = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x04,  //UTF-8 length (int)
            (byte) 0x66, (byte) 0xe2, (byte) 0x88, (byte) 0x9e};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&T", bytesToString(fileContents, expected.length));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, expected.length)));
   }

   private static enum EnumByName implements StaticSerializableEnumByName
   {
      One, Two;
   }

   @Test
   public void writeObject_enumByName() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_enumByName.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(EnumByName.One);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String overhead = "com.github.SkySpiral7.Java.serialization.ObjectStreamWriter_UT$EnumByName|&T";
      final byte[] data = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x03,  //UTF-8 length (int)
            (byte) 79, (byte) 110, (byte) 101};  //"One"
      assertEquals(overhead, bytesToString(fileContents, data.length));
      assertEquals(Arrays.toString(data), Arrays.toString(shortenBytes(fileContents, data.length)));
   }

   @Test
   public void writeObject_enumByName_notEnum() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_enumByName_notEnum.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      class NotEnum implements StaticSerializableEnumByName
      {}

      try
      {
         testObject.writeObject(new NotEnum());
         fail("Didn't throw");
      }
      catch (final ClassCastException actual)
      {
         assertEquals(
               "com.github.SkySpiral7.Java.serialization.ObjectStreamWriter_UT$1NotEnum cannot be cast to java.lang.Enum",
               actual.getMessage());
      }

      testObject.close();
   }

   private static enum EnumByOrdinal implements StaticSerializableEnumByOrdinal
   {
      One, Two, Three, Four;
   }

   @Test
   public void writeObject_enumByOrdinal() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_enumByOrdinal.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(EnumByOrdinal.Four);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String overhead = "com.github.SkySpiral7.Java.serialization.ObjectStreamWriter_UT$EnumByOrdinal|&I";
      final byte[] data = new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x03};
      assertEquals(overhead, bytesToString(fileContents, data.length));
      assertEquals(Arrays.toString(data), Arrays.toString(shortenBytes(fileContents, data.length)));
   }

   @Test
   public void writeObject_enumByOrdinal_notEnum() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_enumByOrdinal_notEnum.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      class NotEnum implements StaticSerializableEnumByOrdinal
      {}

      try
      {
         testObject.writeObject(new NotEnum());
         fail("Didn't throw");
      }
      catch (final ClassCastException actual)
      {
         assertEquals(
               "com.github.SkySpiral7.Java.serialization.ObjectStreamWriter_UT$2NotEnum cannot be cast to java.lang.Enum",
               actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void writeObject_custom() throws IOException
   {
      final class CustomLocal implements StaticSerializable
      {
         boolean wasCalled = false;

         //no reader doesn't matter

         @Override
         public void writeToStream(ObjectStreamWriter out)
         {
            wasCalled = true;
         }
      }

      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_custom.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final CustomLocal data = new CustomLocal();

      testObject.writeObject(data);
      testObject.close();
      assertTrue(data.wasCalled);
   }

   @Test
   public void writeObject_Serializable() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_Serializable.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      final BigInteger data = BigInteger.TEN;
      final byte[] javaData = ObjectStreamWriter.javaSerialize(data);

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("java.math.BigInteger|", bytesToString(fileContents, (javaData.length + 4)));

      final byte[] bytesOfSize = new byte[4];
      System.arraycopy(fileContents, "java.math.BigInteger|".length(), bytesOfSize, 0, 4);
      assertEquals(javaData.length, BitWiseUtil.bigEndianBytesToInteger(bytesOfSize));

      assertEquals(Arrays.toString(javaData), Arrays.toString(shortenBytes(fileContents, javaData.length)));
   }

   @Test
   public void writeObject_throw_unknownClass() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_throw_unknownClass.", ".txt");
      tempFile.deleteOnExit();

      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      try
      {
         testObject.writeObject(new Object());
         fail("Didn't throw");
      }
      catch (final NotSerializableException actual)
      {
         assertEquals("java.lang.Object", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void writeFieldsReflectively() throws IOException
   {
      final class ReflectiveLocal implements StaticSerializable
      {
         private int field = 0xcafe_bead;

         //no reader doesn't matter

         @Override
         public void writeToStream(final ObjectStreamWriter writer)
         {
            writer.writeFieldsReflectively(this);
         }
      }

      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeFieldsReflectively.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final byte[] expected = {(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad};

      testObject.writeObject(new ReflectiveLocal());
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("com.github.SkySpiral7.Java.serialization.ObjectStreamWriter_UT$1ReflectiveLocal|&I", bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   private String bytesToString(final byte[] data, final int bytesToIgnore)
   {
      return new String(data, 0, (data.length - bytesToIgnore), StandardCharsets.UTF_8);
   }

   private byte[] shortenBytes(final byte[] data, final int bytesToKeep)
   {
      final byte[] smallerData = new byte[bytesToKeep];
      System.arraycopy(data, (data.length - bytesToKeep), smallerData, 0, bytesToKeep);
      return smallerData;
   }

   private byte[] subArrayWithLength(final byte[] data, final int start, final int length)
   {
      final byte[] smallerData = new byte[length];
      System.arraycopy(data, start, smallerData, 0, length);
      return smallerData;
   }

}
