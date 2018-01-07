package com.github.skySpiral7.java.staticSerialization;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.strategy.JavaSerializableStrategy;
import com.github.skySpiral7.java.util.BitWiseUtil;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Ignore;
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
         assertEquals("It is not possible to write to a directory (.)", actual.getMessage());
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
      final byte[] expected = {'~', (byte) 0xab};
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
      final byte[] expected = {';'};
      //don't use bytesToString since that assumes the header has UTF-8 encoding
      assertEquals(Arrays.toString(expected), Arrays.toString(FileIoUtil.readBinaryFile(tempFile)));
   }

   @Test
   public void writeObject_byte() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_byte.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Byte data = 2;

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("~", bytesToString(fileContents, 1));
      assertEquals(2, fileContents[1]);
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
      assertEquals("!", bytesToString(fileContents, 2));
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
      assertEquals("@", bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   @Test
   public void writeObject_long() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_long.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Long data = 0xdead_beef__b100_d123L;
      final byte[] expected = {(byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, 0, (byte) 0xd1, 0x23};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("#", bytesToString(fileContents, 8));
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
      assertEquals("%", bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   @Test
   public void writeObject_double() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_double.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);
      final Double data = Double.longBitsToDouble(0xdead_beef__b100_d123L);
      final byte[] expected = {(byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, 0, (byte) 0xd1, 0x23};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("^", bytesToString(fileContents, 8));
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
      assertEquals("+", bytesToString(fileContents, 0));

      FileIoUtil.writeToFile(tempFile, "");

      testObject.writeObject(false);
      testObject.close();
      fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("-", bytesToString(fileContents, 0));
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
      assertEquals("&", bytesToString(fileContents, 2));
      assertEquals("[0, " + 0x66 + "]", Arrays.toString(shortenBytes(fileContents, 2)));

      FileIoUtil.writeToFile(tempFile, "");

      testObject.writeObject('∞');  //infinity sign is BMP non-private
      testObject.close();
      fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("&", bytesToString(fileContents, 2));
      assertEquals("[" + 0x22 + ", " + 0x1e + "]", Arrays.toString(shortenBytes(fileContents, 2)));
   }

   @Test
   public void writeObject_string() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_string.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject("f∞");  //infinity sign is BMP (3 UTF-8 bytes) non-private
      testObject.close();
      final byte[] expected = {0, 0, 0, 4,  //UTF-8 length (int)
            'f', (byte) 0xe2, (byte) 0x88, (byte) 0x9e};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("*", bytesToString(fileContents, expected.length));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, expected.length)));
   }

   @Test
   public void writeObject_objectArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_objectArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Object[]{(byte) 1, (byte) 2});
      testObject.close();
      final byte[] expected = {'[', 1,   //array indicator and dimensions
            'j', 'a', 'v', 'a', '.', 'l', 'a', 'n', 'g', '.', 'O', 'b', 'j', 'e', 'c', 't', ';',  //java.lang.Object
            0, 0, 0, 2,  //length (int)
            '~', 1,  //each element has overhead
            '~', 2};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_boxArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_boxArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Byte[]{1, 2});
      testObject.close();
      final byte[] expected = {'[', 1,   //array indicator and dimensions
            'j', 'a', 'v', 'a', '.', 'l', 'a', 'n', 'g', '.', 'B', 'y', 't', 'e', ';',  //java.lang.Byte
            0, 0, 0, 2,  //length (int)
            '~', 1,  //each element has overhead
            '~', 2};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   @Ignore
   public void writeObject_primitiveArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_primitiveArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new byte[]{1, 2});
      testObject.close();
      final byte[] expected = {'[', 1,   //array indicator and dimensions
            '~',  //byte
            0, 0, 0, 2,  //length (int)
            '~', 1,  //each element has overhead
            '~', 2};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   @Ignore
   public void writeObject_2dArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_2dArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new byte[][]{{1}, null});
      testObject.close();
      final byte[] expected = {'[', 2,   //array indicator and dimensions
            '~',  //byte
            0, 0, 0, 2,  //length (int)
            '[', 1,   //first element
            '~',  //byte
            0, 0, 0, 1,  //length (int)
            '~', 1, ';'};   //second element
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   @Ignore
   public void writeObject_primitiveBooleanArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_primitiveBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new boolean[]{true});
      testObject.close();
      final byte[] expected = {'[', 1,   //array indicator and dimensions
            '+',  //boolean
            0, 0, 0, 1,  //length (int)
            '+'};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_boxBooleanArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_boxBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Boolean[]{false});
      testObject.close();
      final byte[] expected = {'[', 1,   //array indicator and dimensions
            'j', 'a', 'v', 'a', '.', 'l', 'a', 'n', 'g', '.', 'B', 'o', 'o', 'l', 'e', 'a', 'n', ';',  //java.lang.Boolean
            0, 0, 0, 1,  //length (int)
            '-'};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_emptyArray() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_emptyArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Void[0]);
      testObject.close();
      final byte[] expected = {'[', 1,   //array indicator and dimensions
            'j', 'a', 'v', 'a', '.', 'l', 'a', 'n', 'g', '.', 'V', 'o', 'i', 'd', ';',  //java.lang.Void
            0, 0, 0, 0};  //length (int)
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_enum() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_enum.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(RoundingMode.DOWN);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String overhead = "java.math.RoundingMode;";
      final byte[] data = {0, 0, 0, 1};
      assertEquals(overhead, bytesToString(fileContents, data.length));
      assertEquals(Arrays.toString(data), Arrays.toString(shortenBytes(fileContents, data.length)));
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

   private static enum CustomEnum implements StaticSerializable
   {
      One, Two;

      @Override
      public void writeToStream(final ObjectStreamWriter writer)
      {
         writer.writeObject(this.name());
      }
   }

   @Test
   public void writeObject_customEnum() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_customEnum.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(CustomEnum.One);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      final String overhead = "com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter_UT$CustomEnum;*";
      final byte[] data = {0, 0, 0, 3,  //UTF-8 length (int)
            79, 110, 101};  //"One"
      assertEquals(overhead, bytesToString(fileContents, data.length));
      assertEquals(Arrays.toString(data), Arrays.toString(shortenBytes(fileContents, data.length)));
   }

   @Test
   public void writeObject_Serializable() throws IOException
   {
      final File tempFile = File.createTempFile("ObjectStreamWriter_UT.TempFile.writeObject_Serializable.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      final BigInteger data = BigInteger.TEN;
      final byte[] javaData = JavaSerializableStrategy.javaSerialize(data);

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals("java.math.BigInteger;", bytesToString(fileContents, (javaData.length + 4)));

      final byte[] bytesOfSize = new byte[4];
      System.arraycopy(fileContents, "java.math.BigInteger;".length(), bytesOfSize, 0, 4);
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
      assertEquals("com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter_UT$1ReflectiveLocal;@",
            bytesToString(fileContents, 4));
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
