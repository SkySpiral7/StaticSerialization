package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class HeaderSerializableStrategy_UT
{
   //TODO: organize tests. make almost everything an IT but named as UT

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
}
