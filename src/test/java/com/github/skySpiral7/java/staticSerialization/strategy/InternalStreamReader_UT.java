package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.DeserializationException;
import com.github.skySpiral7.java.staticSerialization.exception.InvalidClassException;
import com.github.skySpiral7.java.staticSerialization.exception.NoMoreDataException;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class InternalStreamReader_UT
{
   @Test
   public void constructor_throws()
   {
      try
      {
         new ObjectStreamReader(new File(".")).close();
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("It is not possible to read file contents of a directory", actual.getMessage());
      }
   }

   @Test
   public void readObject_throw_nullInput()
   {
      final ByteReader mockFile = new ByteReader(new byte[0]);
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      boolean didCatch = false;
      try
      {
         testObject.readObject(null);
      }
      catch (final NullPointerException actual)
      {
         didCatch = true;
      }
      assertTrue(didCatch);

      testObject.close();
   }

   @Test
   public void readObject_throw_noData()
   {
      final ByteReader mockFile = new ByteReader(new byte[0]);
      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertFalse(testObject.hasData());
      try
      {
         testObject.readObject(Byte.class);
         fail("Didn't throw");
      }
      catch (final NoMoreDataException actual)
      {
         assertNull(actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_throw_unknownClass()
   {
      final ByteReader mockFile = new ByteReader("java.lang.Object;".getBytes(StandardCharsets.UTF_8));

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(Object.class);
         fail("Didn't throw");
      }
      catch (final NotSerializableException actual)
      {
         assertEquals("java.lang.Object", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_throw_voidClass()
   {
      final ByteReader mockFile = new ByteReader("void;".getBytes(StandardCharsets.UTF_8));

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(void.class);
         fail("Didn't throw");
      }
      catch (final IllegalArgumentException actual)
      {
         assertEquals("There are no instances of void", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_BoxesClassArg_GivenPrimitive()
   {
      final ByteReader mockFile = new ByteReader("-+".getBytes(StandardCharsets.UTF_8));

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertFalse(testObject.readObject(boolean.class));
      assertTrue(testObject.readObject(boolean.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_header_happy()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Byte;");
      fileBuilder.append(new byte[]{2});
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals(2L, testObject.readObject(Byte.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_header_upCast()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Byte;");
      fileBuilder.append(new byte[]{2});
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertEquals(Byte.valueOf((byte) 2), testObject.readObject(Number.class));

      testObject.close();
   }

   @Test
   public void readObject_header_null()
   {
      final ByteReader mockFile = new ByteReader(new byte[]{';'});

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertNull(testObject.readObject(Byte.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_returns_givenId()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(new byte[]{'*', 0, 0, 0, 2});
      fileBuilder.append("hi");
      fileBuilder.append(new byte[]{'\\', 0, 0, 0, 0});
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      String firstObject = testObject.readObject(String.class);
      String secondObject = testObject.readObject(String.class);
      assertSame(firstObject, secondObject);
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_byte()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Byte;");
      final byte[] fileContents = {(byte) 0xde, '~', (byte) 0xad};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals((byte) 0xde, testObject.readObject(Byte.class).byteValue());
      assertEquals((byte) 0xad, testObject.readObject(byte.class).byteValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Short()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Short;");
      fileBuilder.append(new byte[] {0x0a, (byte) 0xfe, '!', 0x2b, (byte) 0xf1});
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals(0x0afeL, testObject.readObject(Short.class).longValue());
      assertEquals(0x2bf1L, testObject.readObject(short.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Integer()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Integer;");
      fileBuilder.append(new byte[]{0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe, '@', 0x0a, 0x1e, (byte) 0xba, (byte) 0xb2});
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals(0x0afe_babeL, testObject.readObject(Integer.class).longValue());
      assertEquals(0x0a1e_bab2L, testObject.readObject(int.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Long()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Long;");
      final byte[] fileContents = {1, 2, 3, 4, 5, 6, 7, 8, '#', 5, 4, 3, 2, 1, 0, 1, 2};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals(0x01020304_05060708L, testObject.readObject(Long.class).longValue());
      assertEquals(0x05040302_01000102L, testObject.readObject(long.class).longValue());
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Float()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Float;");
      final byte[] fileContents = {1, 2, 3, 4, '%', (byte) 0xc1, (byte) 0xd2, (byte) 0xe3, (byte) 0xf4};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals((Float) Float.intBitsToFloat(0x01020304), testObject.readObject(Float.class));
      assertEquals((Float) Float.intBitsToFloat(0xc1d2e3f4), testObject.readObject(float.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Double()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Double;");
      final byte[] fileContents = {1, 2, 3, 4, 5, 6, 7, 8, '^', (byte) 0xa1, (byte) 0xb2, (byte) 0xc3, (byte) 0xd4, (byte) 0xe5,
            (byte) 0xf6, 0x17, 8};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals((Double) Double.longBitsToDouble(0x01020304_05060708L), testObject.readObject(Double.class));
      assertEquals((Double) Double.longBitsToDouble(0xa1b2c3d4_e5f61708L), testObject.readObject(double.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Boolean()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("-+-+java.lang.Boolean;-java.lang.Boolean;+java.lang.Boolean;;");
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertFalse(testObject.readObject(Boolean.class));
      assertTrue(testObject.readObject(Boolean.class));
      assertFalse(testObject.readObject(boolean.class));
      assertTrue(testObject.readObject(boolean.class));
      assertFalse(testObject.readObject(Boolean.class));
      assertTrue(testObject.readObject(Boolean.class));
      assertNull(testObject.readObject(Boolean.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_Character()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.Character;");
      fileBuilder.append(new byte[]{0, 0x66});
      fileBuilder.append("&");
      fileBuilder.append(new byte[]{0x22, 0x1e});
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals('f', testObject.readObject(Character.class).charValue());
      assertEquals(0x221e, testObject.readObject(char.class).charValue());  //infinity sign is BMP non-private
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_String()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.lang.String;");
      fileBuilder.append(new byte[]{0, 0, 0, 4});  //UTF-8 length (int));
      fileBuilder.append("f∞");
      fileBuilder.append("*");  //shorthand
      fileBuilder.append(new byte[]{0, 0, 0, 1});  //UTF-8 length (int));
      fileBuilder.append(new byte[]{0});
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertEquals("f∞", testObject.readObject(String.class));  //infinity sign is BMP (3 UTF-8 bytes) non-private
      assertEquals("\u0000", testObject.readObject(String.class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_objectArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("[");
      fileBuilder.append(new byte[]{1});   //array indicator and dimensions
      fileBuilder.append("java.lang.Object;");
      final byte[] fileContents = {0, 0, 0, 2,  //length (int)
            '~', 1,  //each element has header
            '~', 2};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final Object[] expected = {(byte) 1, (byte) 2};

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Object[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_boxArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("[");
      fileBuilder.append(new byte[]{1});   //array indicator and dimensions
      fileBuilder.append("java.lang.Byte;");
      final byte[] fileContents = {0, 0, 0, 2,  //length (int)
            '~', 1,  //each element has header
            '~', 2};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final Byte[] expected = {1, 2};

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Byte[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_primitiveArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      final byte[] fileContents = {']', 1,   //array indicator and dimensions
            '~',  //byte
            0, 0, 0, 2,  //length (int)
            1, 2};  //elements of a primitive array do not have headers
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final byte[] expected = {1, 2};

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(byte[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_2dArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(new byte[]{'[', 2});   //root array indicator and dimensions
      fileBuilder.append("java.lang.Byte;");  //root component
      fileBuilder.append(new byte[]{0, 0, 0, 2});   //root length (int)
      fileBuilder.append("~");  //root[0] component
      fileBuilder.append(new byte[]{0, 0, 0, 1});   //root[0] length (int)
      fileBuilder.append(new byte[]{'~', 1});   //root[0][0] data with header
      fileBuilder.append(";");   //root[1] is null
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final Byte[][] expected = {{1}, null};

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Byte[][].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_2dPrimitiveArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(new byte[]{']', 2});   //root array indicator and dimensions
      fileBuilder.append("java.lang.Byte;");  //root component
      fileBuilder.append(new byte[]{0, 0, 0, 2});   //root length (int)
      fileBuilder.append("~");  //root[0] component
      fileBuilder.append(new byte[]{0, 0, 0, 1});   //root[0] length (int)
      fileBuilder.append(new byte[]{1});   //root[0][0] data (no header)
      fileBuilder.append(";");  //root[1]=null
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final byte[][] expected = {{1}, null};

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(byte[][].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_primitiveBooleanArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      final byte[] fileContents = {']', 1,   //array indicator and dimensions
            '+',  //boolean
            0, 0, 0, 1,  //length (int)
            '+'};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final boolean[] expected = {true};

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(boolean[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_boxBooleanArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("[");
      fileBuilder.append(new byte[]{1});   //array indicator and dimensions
      fileBuilder.append("+");
      final byte[] fileContents = {0, 0, 0, 1,  //length (int)
            '-'};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final Boolean[] expected = {false};

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Boolean[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_emptyArray()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("[");
      fileBuilder.append(new byte[]{1});   //array indicator and dimensions
      fileBuilder.append("java.lang.Void;");
      final byte[] fileContents = {0, 0, 0, 0};  //length (int)
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());
      final Void[] expected = new Void[0];

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertTrue(testObject.hasData());
      assertArrayEquals(expected, testObject.readObject(Void[].class));
      assertFalse(testObject.hasData());

      testObject.close();
   }

   @Test
   public void readObject_custom_happy()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      final String header = "com.github.skySpiral7.java.staticSerialization.testClasses.SimpleHappy;";
      fileBuilder.append(header);
      final byte[] fileContents = {'@', 0, 0, 0, 4};
      fileBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      final SimpleHappy readData = testObject.readObject(SimpleHappy.class);
      assertEquals(4, readData.smileyStickersCount);
      assertFalse(testObject.hasData());

      testObject.close();
   }

   public static enum CustomEnum implements StaticSerializable
   {
      One, Two;

      public static CustomEnum readFromStream(final ObjectStreamReader reader)
      {
         final String name = reader.readObject(String.class);
         return CustomEnum.valueOf(name);
      }

      @Override
      public void writeToStream(final ObjectStreamWriter writer)
      {
         fail("Should not be called");
      }
   }

   @Test
   public void readObject_customEnum()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(CustomEnum.class.getName());
      fileBuilder.append(new byte[]{';', '*', 0, 0, 0, 3});
      fileBuilder.append("One");
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertSame(CustomEnum.One, testObject.readObject(CustomEnum.class));

      testObject.close();
   }

   private static abstract class CustomPrivateClass implements StaticSerializable
   {}

   @Test
   public void readObject_custom_throw_privateClass()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(CustomPrivateClass.class.getName());
      fileBuilder.append(";");
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(CustomPrivateClass.class);
         fail("Didn't throw");
      }
      catch (final InvalidClassException actual)
      {
         assertEquals("com.github.skySpiral7.java.staticSerialization.strategy.InternalStreamReader_UT$CustomPrivateClass"
                      + " must be public for me to use it", actual.getMessage());
      }

      testObject.close();
   }

   public abstract class NoReader implements StaticSerializable
   {}  //abstract and no writer doesn't matter

   @Test
   public void readObject_custom_throw_noReader()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(NoReader.class.getName());
      fileBuilder.append(";");
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(NoReader.class);
         fail("Didn't throw");
      }
      catch (final InvalidClassException actual)
      {
         assertEquals("com.github.skySpiral7.java.staticSerialization.strategy.InternalStreamReader_UT$NoReader"
                      + " implements StaticSerializable but doesn't define readFromStream", actual.getMessage());
      }

      testObject.close();
   }

   public abstract static class NonPublicReader implements StaticSerializable
   {
      @SuppressWarnings("unused")
      protected static NonPublicReader readFromStream(final ObjectStreamReader in)
      {
         return null;
      }
   }

   @Test
   public void readObject_custom_throw_nonPublic()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(NonPublicReader.class.getName());
      fileBuilder.append(";");
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(NonPublicReader.class);
         fail("Didn't throw");
      }
      catch (final InvalidClassException actual)
      {
         assertEquals("com.github.skySpiral7.java.staticSerialization.strategy.InternalStreamReader_UT$NonPublicReader.readFromStream"
                      + " must be public static", actual.getMessage());
      }

      testObject.close();
   }

   public abstract class LocalNonStaticReader implements StaticSerializable
   {
      @SuppressWarnings("unused")
      public LocalNonStaticReader readFromStream(final ObjectStreamReader in)
      {
         return null;
      }
   }

   @Test
   public void readObject_custom_throw_nonStatic()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(LocalNonStaticReader.class.getName());
      fileBuilder.append(";");
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(LocalNonStaticReader.class);
         fail("Didn't throw");
      }
      catch (final InvalidClassException actual)
      {
         assertEquals("com.github.skySpiral7.java.staticSerialization.strategy.InternalStreamReader_UT$LocalNonStaticReader.readFromStream"
                      + " must be public static", actual.getMessage());
      }

      testObject.close();
   }

   public abstract static class ThrowingReader implements StaticSerializable
   {
      @SuppressWarnings("unused")
      public static ThrowingReader readFromStream(final ObjectStreamReader in)
      {
         throw new UnsupportedOperationException();
      }
   }

   @Test
   public void readObject_custom_throw_throwingReader()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append(ThrowingReader.class.getName());
      fileBuilder.append(";");
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      try
      {
         testObject.readObject(ThrowingReader.class);
         fail("Didn't throw");
      }
      catch (final DeserializationException actual)
      {
         assertEquals(InvocationTargetException.class, actual.getCause().getClass());
         assertEquals(UnsupportedOperationException.class, actual.getCause().getCause().getClass());
      }

      testObject.close();
   }

   @Test
   public void readObject_Serializable()
   {
      final ByteAppender fileBuilder = new ByteAppender();
      fileBuilder.append("java.math.BigInteger;");
      final BigInteger data = BigInteger.TEN;
      byte[] javaData = JavaSerializableStrategy.javaSerialize(data);
      fileBuilder.append(BitWiseUtil.toBigEndianBytes(javaData.length, 4));
      fileBuilder.append(javaData);
      fileBuilder.append("java.math.BigInteger;");
      javaData = JavaSerializableStrategy.javaSerialize(null);
      fileBuilder.append(BitWiseUtil.toBigEndianBytes(javaData.length, 4));
      fileBuilder.append(javaData);
      final ByteReader mockFile = new ByteReader(fileBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertEquals(data, testObject.readObject());
      assertNull(testObject.readObject());

      testObject.close();
   }
}
