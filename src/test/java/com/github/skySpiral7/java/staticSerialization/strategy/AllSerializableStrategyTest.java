package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
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

import java.util.Arrays;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

class AllSerializableStrategyTest
{
   private AllSerializableStrategy testObject;

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

      final HeaderInformation<?> actual = testObject.readHeader(null, null, Boolean.class, false);
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

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, Byte.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, Byte.class, false);
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

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, Short.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, Short.class, false);
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

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, Integer.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, Integer.class, false);
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

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, Long.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, Long.class, false);
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

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, Float.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, Float.class, false);
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

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, Double.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, Double.class, false);
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

      final HeaderInformation<?> actual1 = testObject.readHeader(null, null, Character.class, false);
      assertEquals(expected, actual1);
      final HeaderInformation<?> actual2 = testObject.readHeader(null, null, Character.class, false);
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

      final HeaderInformation<?> actual = testObject.readHeader(null, null, Byte[].class, false);

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

      final HeaderInformation<?> actual = testObject.readHeader(null, null, Object.class, true);
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

      final HeaderInformation<?> actual = testObject.readHeader(null, null, Object.class, true);

      assertEquals(expected, actual);
      reader.close();
   }

   private void init(final EasyReader reader, final ObjectReaderRegistry registry)
   {
      final StrategyInstances strategyInstances = new StrategyInstances(null, null, reader, registry,
         new UtilInstances());
      testObject = strategyInstances.getAllSerializableStrategy();
   }

   private void init(final EasyAppender appender, final ObjectWriterRegistry registry)
   {
      final StrategyInstances strategyInstances = new StrategyInstances(null, null, appender, registry,
         new UtilInstances());
      testObject = strategyInstances.getAllSerializableStrategy();
   }
}
