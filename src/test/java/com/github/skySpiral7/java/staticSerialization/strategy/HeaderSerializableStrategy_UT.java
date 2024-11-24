package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.HeaderInformation;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectReaderRegistry;
import com.github.skySpiral7.java.staticSerialization.internal.ObjectWriterRegistry;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.stream.EasyAppender;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import com.github.skySpiral7.java.staticSerialization.util.UtilInstances;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

public class HeaderSerializableStrategy_UT
{
   //TODO: organize tests. make almost everything an IT but named as UT
   private HeaderSerializableStrategy testObject;

   @Test
   public void readHeader_primitiveArrayElementsHaveNoHeader()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{']', 1, '~'});  //header
      expectedBuilder.append(new byte[]{0, 0, 0, 2});  //length
      final byte[] expectedData = {2, 5};
      expectedBuilder.append(expectedData);
      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

      final byte[] actual = streamReader.readObject(byte[].class);

      assertEquals(Arrays.toString(expectedData), Arrays.toString(actual));
      streamReader.close();
   }

   @Test
   public void readHeader_inheritType()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 2, '~'});   //root array indicator, dimensions, component
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //root length (int)
      expectedBuilder.append(new byte[]{'?'});  //root[0] inherits type
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //root[0] length (int)
      expectedBuilder.append(new byte[]{'?', 1, StringSerializableStrategy.TERMINATOR});   //root[0][0] data inherits type, root[0][1] is null (not same type)
      final Byte[][] expected = {{1, null}};
      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

      final Byte[][] actual = streamReader.readObject(Byte[][].class);

      assertThat(actual, is(expected));
      streamReader.close();
   }

   @Test
   public void readHeader_inheritTypeIsNotRequired()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1});   //array indicator, dimensions
      expectedBuilder.append("java.lang.Object");   //component
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //length (int)
      expectedBuilder.append(new byte[]{'~', 1});   //data with header (inherit wouldn't be a supported type here)
      final Object[] expected = {(byte) 1};
      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

      final Object[] actual = streamReader.readObject(Object[].class);

      assertThat(actual, is(expected));
      streamReader.close();
   }

   @Test
   public void readHeader_throws_whenInheritOutsideOfArray()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("?1");
      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

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
   public void readHeader_throws_whenNoArrayDimensions()
   {
      final EasyReader reader = new ByteReader(new byte[]{'['});
      init(reader, null);

      try
      {
         testObject.readHeader(null, null, null, false);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: no array dimensions", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_throws_whenNoArrayComponent()
   {
      final EasyReader reader = new ByteReader(new byte[]{'[', 'a'});  //'a' is 97 dimensions
      init(reader, null);

      try
      {
         testObject.readHeader(null, null, null, false);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: no array component type", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_throws_whenArrayComponentIsNull()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("[a");  //'a' is 97 dimensions
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

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
   public void readHeader_throws_whenArrayComponentIsFalse()
   {
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("[a-");  //'a' is 97 dimensions
      final ByteReader mockFile = new ByteReader(expectedBuilder.getAllBytes());
      final ObjectStreamReader streamReader = new ObjectStreamReader(mockFile);

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
   public void readHeader_returns_givenBooleanArrayInStream()
   {
      final EasyReader reader = new ByteReader(new byte[]{'[', 1, '+'});
      init(reader, null);
      final HeaderInformation<Boolean> expected = new HeaderInformation<>((byte) '[', Boolean.class.getName(), Boolean.class, null, 1, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returns_givenPrimitiveBooleanArrayInStream()
   {
      final EasyReader reader = new ByteReader(new byte[]{']', 1, '+'});
      init(reader, null);
      final HeaderInformation<Boolean> expected = new HeaderInformation<>((byte) ']', Boolean.class.getName(), Boolean.class, null, 1, true);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsNullInfo_givenNullInStream()
   {
      final EasyReader reader = new ByteReader(new byte[]{StringSerializableStrategy.TERMINATOR});
      init(reader, null);
      final HeaderInformation<?> expected = new HeaderInformation<>(StringSerializableStrategy.TERMINATOR, null, Object.class, null, 0, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsTrueInfo_givenTrueInStream()
   {
      final EasyReader reader = new ByteReader(new byte[]{'+'});
      init(reader, null);
      final HeaderInformation<Boolean> expected = new HeaderInformation<>((byte) 'j', "java.lang.Boolean", null, Boolean.TRUE, 0, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsFalseInfo_givenFalseInStream()
   {
      final EasyReader reader = new ByteReader(new byte[]{'-'});
      init(reader, null);
      final HeaderInformation<Boolean> expected = new HeaderInformation<>((byte) 'j', "java.lang.Boolean", null, Boolean.FALSE, 0, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_returnsBooleanInfo_givenBooleanObjectInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Boolean");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Boolean> expected = new HeaderInformation<>((byte) 'j', "java.lang.Boolean", null, null, 0, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual);

      reader.close();
   }

   @Test
   public void readHeader_returnsByteInfo_givenByteInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Byte");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '~');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Byte> expected = new HeaderInformation<>((byte) 'j', "java.lang.Byte", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsShortInfo_givenShortInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Short");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '!');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Short> expected = new HeaderInformation<>((byte) 'j', "java.lang.Short", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsIntegerInfo_givenIntegerInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Integer");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '@');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Integer> expected = new HeaderInformation<>((byte) 'j', "java.lang.Integer", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsLongInfo_givenLongInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Long");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '#');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Long> expected = new HeaderInformation<>((byte) 'j', "java.lang.Long", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsFloatInfo_givenFloatInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Float");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '%');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Float> expected = new HeaderInformation<>((byte) 'j', "java.lang.Float", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsDoubleInfo_givenDoubleInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Double");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '^');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Double> expected = new HeaderInformation<>((byte) 'j', "java.lang.Double", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsCharacterInfo_givenCharacterInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Character");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '\'');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Character> expected = new HeaderInformation<>((byte) 'j', "java.lang.Character", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returnsStringInfo_givenStringInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.String");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      inputBuilder.append((byte) '"');
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<String> expected = new HeaderInformation<>((byte) 'j', "java.lang.String", null, null, 0, false);

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, String.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, String.class, false);
      assertEquals(expected, actual2);

      reader.close();
   }

   @Test
   public void readHeader_returns_givenByteArrayInStream()
   {
      final EasyReader reader = new ByteReader(new byte[]{'[', 1, '~'});
      init(reader, null);
      final HeaderInformation<Byte> expected = new HeaderInformation<>((byte) 'j', Byte.class.getName(), Byte.class, null, 1, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_throws_whenIdMissing()
   {
      ObjectReaderRegistry registry = new ObjectReaderRegistry();
      registry.reserveIdForLater();
      registry.registerObject("hi");

      final EasyReader reader = new ByteReader(new byte[]{'&'});
      init(reader, registry);

      try
      {
         testObject.readHeader(null, null, null, false);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Incomplete header: id type but no id", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_throws_whenNoMatchingId()
   {
      ObjectReaderRegistry registry = new ObjectReaderRegistry();
      registry.reserveIdForLater();

      final EasyReader reader = new ByteReader(new byte[]{'&', 0, 0, 0, 0});
      init(reader, registry);

      try
      {
         testObject.readHeader(null, null, null, false);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("id not found", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_returns_givenId()
   {
      ObjectReaderRegistry registry = new ObjectReaderRegistry();
      registry.reserveIdForLater();
      String objectValue = "hi";
      registry.registerObject(objectValue);

      final EasyReader reader = new ByteReader(new byte[]{'&', 0, 0, 0, 0});
      init(reader, registry);
      final HeaderInformation<String> expected = new HeaderInformation<>((byte) 'j', String.class.getName(), null, objectValue, 0, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void readHeader_throws_whenHeaderNotTerminated()
   {
      final EasyReader reader = new ByteReader(new byte[]{'j'});
      init(reader, null);

      try
      {
         testObject.readHeader(null, null, null, false);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("String data not terminated", actual.getMessage());
      }

      reader.close();
   }

   @Test
   public void readHeader_returnsObjectInfo_givenObjectInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append("java.lang.Object");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Object> expected = new HeaderInformation<>((byte) 'j', "java.lang.Object", null, null, 0, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);
      assertEquals(expected, actual);

      reader.close();
   }

   @Test
   public void readHeader_returns_givenObjectArrayInStream()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      inputBuilder.append(new byte[]{'[', 1});
      inputBuilder.append("java.lang.Object");
      inputBuilder.append(StringSerializableStrategy.TERMINATOR);
      final EasyReader reader = new ByteReader(inputBuilder.getAllBytes());
      init(reader, null);
      final HeaderInformation<Object> expected = new HeaderInformation<>((byte) 'j', Object.class.getName(), null, null, 1, false);

      final HeaderInformation<?> actual = testObject.readHeader(null, null, null, false);

      assertEquals(expected, actual);
      reader.close();
   }

   @Test
   public void writeObject_header()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject((byte) 0xab);
      testObject.close();
      final byte[] expected = {'~', (byte) 0xab};
      //don't use bytesToString since that assumes the header has UTF-8 encoding
      assertEquals(Arrays.toString(expected), Arrays.toString(mockFile.getAllBytes()));
   }

   @Test
   public void writeObject_header_null()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(null);
      testObject.close();
      final byte[] expected = {StringSerializableStrategy.TERMINATOR};
      //don't use bytesToString since that assumes the header has UTF-8 encoding
      assertEquals(Arrays.toString(expected), Arrays.toString(mockFile.getAllBytes()));
   }

   @Test
   public void writeObject_id()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      String reusedValue = "f";
      testObject.writeObject(reusedValue);
      testObject.writeObject(reusedValue);
      testObject.close();
      final byte[] expected = {
         '"',  //short hand for String
         'f',  //data
         StringSerializableStrategy.TERMINATOR,
         '&',  //id type
         0, 0, 0, 0  //id
      };
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_byte()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final Byte data = 2;

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals("~", bytesToString(fileContents, 1));
      assertEquals(2, fileContents[1]);
   }

   @Test
   public void writeObject_short()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final Short data = (short) 0xcafe;
      final byte[] expected = {(byte) 0xca, (byte) 0xfe};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals("!", bytesToString(fileContents, 2));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 2)));
   }

   @Test
   public void writeObject_int()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final Integer data = 0xcafe_bead;
      final byte[] expected = {(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals("@", bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   @Test
   public void writeObject_long()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final Long data = 0xdead_beef__b100_d123L;
      final byte[] expected = {(byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, 0, (byte) 0xd1, 0x23};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals("#", bytesToString(fileContents, 8));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 8)));
   }

   @Test
   public void writeObject_float()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final Float data = Float.intBitsToFloat(0xcafe_bead);
      final byte[] expected = {(byte) 0xca, (byte) 0xfe, (byte) 0xbe, (byte) 0xad};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals("%", bytesToString(fileContents, 4));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 4)));
   }

   @Test
   public void writeObject_double()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final Double data = Double.longBitsToDouble(0xdead_beef__b100_d123L);
      final byte[] expected = {(byte) 0xde, (byte) 0xad, (byte) 0xbe, (byte) 0xef, (byte) 0xb1, 0, (byte) 0xd1, 0x23};

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals("^", bytesToString(fileContents, 8));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, 8)));
   }

   @Test
   public void writeHeader_boolean()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter streamWriter = new ObjectStreamWriter(mockFile);

      streamWriter.writeObject(true);
      streamWriter.writeObject(false);
      streamWriter.close();
      final byte[] expected = {'+', '-'};
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_char()
   {
      ByteAppender mockFile = new ByteAppender();
      ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject('f');
      testObject.flush();
      byte[] fileContents = mockFile.getAllBytes();
      assertEquals("'", bytesToString(fileContents, 2));
      assertEquals("[0, " + 0x66 + "]", Arrays.toString(shortenBytes(fileContents, 2)));

      mockFile = new ByteAppender();
      testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject('∞');  //infinity sign is BMP non-private
      testObject.close();
      fileContents = mockFile.getAllBytes();
      assertEquals("'", bytesToString(fileContents, 2));
      assertEquals("[" + 0x22 + ", " + 0x1e + "]", Arrays.toString(shortenBytes(fileContents, 2)));
   }

   @Test
   public void writeObject_string()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject("f∞");  //infinity sign is BMP (3 UTF-8 bytes) non-private
      testObject.close();
      final byte[] expected = {
         'f', (byte) 0xe2, (byte) 0x88, (byte) 0x9e,
         StringSerializableStrategy.TERMINATOR
      };
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals("\"", bytesToString(fileContents, expected.length));
      assertEquals(Arrays.toString(expected), Arrays.toString(shortenBytes(fileContents, expected.length)));
   }

   @Test
   public void writeObject_objectArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new Object[]{(byte) 1, (byte) 2});
      testObject.close();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1});   //array indicator and dimensions
      expectedBuilder.append("java.lang.Object");
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //length (int)
      expectedBuilder.append(new byte[]{'~', 1});
      expectedBuilder.append(new byte[]{'~', 2});
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expectedBuilder.getAllBytes()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_boxArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new Byte[]{1, 2});
      testObject.close();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1, '~'});   //array indicator, dimensions, component
      expectedBuilder.append(new byte[]{0, 0, 0, 2});   //length (int)
      expectedBuilder.append(new byte[]{'?', 1});
      expectedBuilder.append(new byte[]{'?', 2});
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expectedBuilder.getAllBytes()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_stringArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new String[0]);
      testObject.close();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1, '"'});   //array indicator, dimensions, component
      expectedBuilder.append(new byte[]{0, 0, 0, 0});   //length (int)
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expectedBuilder.getAllBytes()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_primitiveArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new byte[]{1, 2});
      testObject.close();
      final byte[] expected = {
         ']', 1,   //array indicator and dimensions
         '~',  //byte
         0, 0, 0, 2,  //length (int)
         1, 2  //primitive elements have no header
      };
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_2dArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new Byte[][]{{1}, null});
      testObject.close();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 2, '~'});  //root array indicator, dimensions, component
      expectedBuilder.append(new byte[]{0, 0, 0, 2});  //root length (int)
      expectedBuilder.append(new byte[]{'?'});  //root[0] inherits type, dimensions, and component
      expectedBuilder.append(new byte[]{0, 0, 0, 1});  //root[0] length (int)
      expectedBuilder.append(new byte[]{'?', 1});  //root[0][0] data with header
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);  //root[1] is null
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expectedBuilder.getAllBytes()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_primitiveBooleanArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new boolean[]{true});
      testObject.close();
      final byte[] expected = {
         ']', 1,   //array indicator and dimensions
         '+',  //boolean
         0, 0, 0, 1,  //length (int)
         '+'
      };
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_boxBooleanArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new Boolean[]{false});
      testObject.close();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1, '+'});   //array indicator, dimensions, component
      expectedBuilder.append(new byte[]{0, 0, 0, 1});   //length (int)
      expectedBuilder.append(new byte[]{'-'});
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expectedBuilder.getAllBytes()), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_emptyArray()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      testObject.writeObject(new Void[0]);
      testObject.close();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(new byte[]{'[', 1});   //array indicator and dimensions
      expectedBuilder.append("java.lang.Void");
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append(new byte[]{0, 0, 0, 0});   //length (int)
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expectedBuilder.getAllBytes()), Arrays.toString(fileContents));
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

   private void init(final EasyReader reader, final ObjectReaderRegistry registry)
   {
      final StrategyInstances strategyInstances = new StrategyInstances(null, null, reader, registry,
         new UtilInstances());
      testObject = strategyInstances.getHeaderSerializableStrategy();
   }

   private void init(final EasyAppender appender, final ObjectWriterRegistry registry)
   {
      final StrategyInstances strategyInstances = new StrategyInstances(null, null, appender, registry,
         new UtilInstances());
      testObject = strategyInstances.getHeaderSerializableStrategy();
   }
}
