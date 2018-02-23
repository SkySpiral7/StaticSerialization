package com.github.skySpiral7.java.staticSerialization.strategy;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.fileWrapper.AsynchronousFileReader;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class HeaderSerializableStrategy_UT
{
   //TODO: organize tests. make almost everything an IT but named as UT

   @Test
   public void readHeader_primitiveArrayElementsHaveNoHeader() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_primitiveArrayElementsHaveNoHeader.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, new byte[]{']', 1, '~'});  //header
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 2});  //length
      final byte[] expectedData = {2, 5};
      FileIoUtil.appendToFile(tempFile, expectedData);
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

      final byte[] actual = streamReader.readObject(byte[].class);

      assertEquals(Arrays.toString(expectedData), Arrays.toString(actual));
      streamReader.close();
   }

   @Test
   public void readHeader_inheritType() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_inheritType.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, new byte[]{'[', 2, '~'});   //root array indicator, dimensions, component
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 1});   //root length (int)
      FileIoUtil.appendToFile(tempFile, new byte[]{'?'});  //root[0] inherits type
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 2});   //root[0] length (int)
      FileIoUtil.appendToFile(tempFile, new byte[]{'?', 1, ';'});   //root[0][0] data inherits type, root[0][1] is null (not same type)
      final Byte[][] expected = {{1, null}};
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

      final Byte[][] actual = streamReader.readObject(Byte[][].class);

      assertArrayEquals(expected, actual);
      streamReader.close();
   }

   @Test
   public void readHeader_inheritTypeIsNotRequired() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_inheritTypeIsNotRequired.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, new byte[]{'[', 1});   //array indicator, dimensions
      FileIoUtil.appendToFile(tempFile, "java.lang.Object;".getBytes(StandardCharsets.UTF_8));   //component
      FileIoUtil.appendToFile(tempFile, new byte[]{0, 0, 0, 1});   //length (int)
      FileIoUtil.appendToFile(tempFile, new byte[]{'~', 1});   //data with header (inherit wouldn't be a supported type here)
      final Object[] expected = {(byte) 1};
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

      final Object[] actual = streamReader.readObject(Object[].class);

      assertArrayEquals(expected, actual);
      streamReader.close();
   }

   @Test
   public void readHeader_throws_whenInheritOutsideOfArray() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_throws_whenInheritOutsideOfArray.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "?1");
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

      try
      {
         streamReader.readObject();
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Only array elements can inherit type", actual.getMessage());
      }

      streamReader.close();
   }

   @Test
   public void readHeader_throws_whenNoArrayDimensions() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_throws_whenNoArrayDimensions.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readHeader(reader, null);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: no array dimensions", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_throws_whenNoArrayComponent() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_throws_whenNoArrayComponent.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[a");  //'a' is a lot of dimensions
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readHeader(reader, null);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: no array component type", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_throws_whenArrayComponentIsNull() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_throws_whenArrayComponentIsNull.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[a;");  //'a' is 97 dimensions
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

      try
      {
         streamReader.readObject();
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("header's array component type can't be null", actual.getMessage());
      }

      streamReader.close();
   }

   @Test
   public void readHeader_throws_whenArrayComponentIsFalse() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_throws_whenArrayComponentIsFalse.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "[a-");  //'a' is 97 dimensions
      final ObjectStreamReader streamReader = new ObjectStreamReader(tempFile);

      try
      {
         streamReader.readObject();
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("header's array component type can't be false", actual.getMessage());
      }

      streamReader.close();
   }

   @Test
   public void readHeader_returns_givenBooleanArrayInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returns_givenBooleanArrayInStream.",
            ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, new byte[]{'[', 1, '+'});
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(Boolean.class.getName(), null, 1, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returns_givenPrimitiveBooleanArrayInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readHeader_returns_givenPrimitiveBooleanArrayInStream.", ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "]");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "+");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(Boolean.class.getName(), null, 1, true);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsNullInfo_givenNullInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsNullInfo_givenNullInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, ";");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(null, null, 0, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsTrueInfo_givenTrueInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsTrueInfo_givenTrueInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "+");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", Boolean.TRUE, 0, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsFalseInfo_givenFalseInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsFalseInfo_givenFalseInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "-");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", Boolean.FALSE, 0, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsBooleanInfo_givenBooleanObjectInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readHeader_returnsBooleanInfo_givenBooleanObjectInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Boolean;");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Boolean", null, 0, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual);

      reader.close();
   }

   @Test
   public void readHeader_returnsByteInfo_givenByteInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsByteInfo_givenByteInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Byte;~");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Byte", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsShortInfo_givenShortInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsShortInfo_givenShortInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Short;!");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Short", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsIntegerInfo_givenIntegerInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readHeader_returnsIntegerInfo_givenIntegerInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Integer;@");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Integer", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsLongInfo_givenLongInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsLongInfo_givenLongInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Long;#");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Long", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsFloatInfo_givenFloatInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsFloatInfo_givenFloatInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Float;%");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Float", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsDoubleInfo_givenDoubleInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsDoubleInfo_givenDoubleInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Double;^");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Double", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsCharacterInfo_givenCharacterInStream() throws Exception
   {
      final File tempFile = File.createTempFile(
            "HeaderSerializableStrategy_UT.TempFile.readHeader_returnsCharacterInfo_givenCharacterInStream.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Character;&");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Character", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsStringInfo_givenStringInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsStringInfo_givenStringInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.String;*");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.String", null, 0, false);

      final HeaderInformation actual1 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual1);
      final HeaderInformation actual2 = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returns_givenByteArrayInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returns_givenByteArrayInStream.",
            ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "~");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(Byte.class.getName(), null, 1, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_throws_whenHeaderNoTerminated() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_throws_whenHeaderNoTerminated.", ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "j");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);

      try
      {
         HeaderSerializableStrategy.readHeader(reader, null);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: class name not terminated", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_returnsObjectInfo_givenObjectInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returnsObjectInfo_givenObjectInStream.",
            ".txt");
      tempFile.deleteOnExit();
      FileIoUtil.writeToFile(tempFile, "java.lang.Object;");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation("java.lang.Object", null, 0, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);
      assertEquals(expected, actual);

      reader.close();
   }

   @Test
   public void readHeader_returns_givenObjectArrayInStream() throws Exception
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.readHeader_returns_givenObjectArrayInStream.",
            ".txt");
      tempFile.deleteOnExit();

      FileIoUtil.writeToFile(tempFile, "[");
      FileIoUtil.appendToFile(tempFile, new byte[]{1});
      FileIoUtil.appendToFile(tempFile, "java.lang.Object;");
      final AsynchronousFileReader reader = new AsynchronousFileReader(tempFile);
      final HeaderInformation expected = new HeaderInformation(Object.class.getName(), null, 1, false);

      final HeaderInformation actual = HeaderSerializableStrategy.readHeader(reader, null);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void writeObject_header() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_header.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject((byte) 0xab);
      testObject.close();
      final byte[] expected = {'~', (byte) 0xab};
      //don't use bytesToString since that assumes the header has UTF-8 encoding
      assertEquals(Arrays.toString(expected), Arrays.toString(FileIoUtil.readBinaryFile(tempFile)));
   }

   @Test
   public void writeObject_header_null() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_header_null.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_byte.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_short.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_int.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_long.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_float.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_double.", ".txt");
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
   public void writeHeader_boolean() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeHeader_boolean.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter streamWriter = new ObjectStreamWriter(tempFile);

      streamWriter.writeObject(true);
      streamWriter.writeObject(false);
      streamWriter.close();
      final byte[] expected = {'+', '-'};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_char() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_char.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_string.", ".txt");
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
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_objectArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Object[]{(byte) 1, (byte) 2});
      testObject.close();
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 1});   //array indicator and dimensions
      baos.write("java.lang.Object;".getBytes(StandardCharsets.UTF_8));
      baos.write(new byte[]{0, 0, 0, 2});   //length (int)
      baos.write(new byte[]{'~', 1});
      baos.write(new byte[]{'~', 2});
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(baos.toByteArray()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_boxArray() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_boxArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Byte[]{1, 2});
      testObject.close();
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 1, '~'});   //array indicator, dimensions, component
      baos.write(new byte[]{0, 0, 0, 2});   //length (int)
      baos.write(new byte[]{'?', 1});
      baos.write(new byte[]{'?', 2});
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(baos.toByteArray()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_stringArray() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_stringArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new String[0]);
      testObject.close();
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 1, '*'});   //array indicator, dimensions, component
      baos.write(new byte[]{0, 0, 0, 0});   //length (int)
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(baos.toByteArray()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_primitiveArray() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_primitiveArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new byte[]{1, 2});
      testObject.close();
      final byte[] expected = {']', 1,   //array indicator and dimensions
            '~',  //byte
            0, 0, 0, 2,  //length (int)
            1, 2};  //primitive elements have no header
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_2dArray() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_2dArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Byte[][]{{1}, null});
      testObject.close();
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 2, '~'});   //root array indicator, dimensions, component
      baos.write(new byte[]{0, 0, 0, 2});   //root length (int)
      baos.write(new byte[]{'?'});   //root[0] inherits type, dimensions, and component
      baos.write(new byte[]{0, 0, 0, 1});   //root[0] length (int)
      baos.write(new byte[]{'?', 1});   //root[0][0] data with header
      baos.write(';');   //root[1] is null
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(baos.toByteArray()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_primitiveBooleanArray() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_primitiveBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new boolean[]{true});
      testObject.close();
      final byte[] expected = {']', 1,   //array indicator and dimensions
            '+',  //boolean
            0, 0, 0, 1,  //length (int)
            '+'};
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_boxBooleanArray() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_boxBooleanArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Boolean[]{false});
      testObject.close();
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 1, '+'});   //array indicator, dimensions, component
      baos.write(new byte[]{0, 0, 0, 1});   //length (int)
      baos.write(new byte[]{'-'});
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(baos.toByteArray()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_emptyArray() throws IOException
   {
      final File tempFile = File.createTempFile("HeaderSerializableStrategy_UT.TempFile.writeObject_emptyArray.", ".txt");
      tempFile.deleteOnExit();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(tempFile);

      testObject.writeObject(new Void[0]);
      testObject.close();
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      baos.write(new byte[]{'[', 1});   //array indicator and dimensions
      baos.write("java.lang.Void;".getBytes(StandardCharsets.UTF_8));
      baos.write(new byte[]{0, 0, 0, 0});   //length (int)
      final byte[] fileContents = FileIoUtil.readBinaryFile(tempFile);
      assertEquals(Arrays.toString(baos.toByteArray()), Arrays.toString(fileContents));
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
}
