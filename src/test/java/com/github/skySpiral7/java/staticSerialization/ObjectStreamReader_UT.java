package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import com.github.skySpiral7.java.util.FileIoUtil;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.math.RoundingMode;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class ObjectStreamReader_UT
{
   @Test
   public void readBytes_throw() throws Exception
   {
      //tests com.github.skySpiral7.java.staticSerialization.strategy.ShortSerializableStrategy
      final File tempFile = File.createTempFile("ObjectStreamReader_UT.TempFile.readBytes_throw.", ".txt");
      tempFile.deleteOnExit();
      final byte[] fileContents = {'!', 0x0a};
      FileIoUtil.writeToFile(tempFile, fileContents);

      final ObjectStreamReader testObject = new ObjectStreamReader(tempFile);
      try
      {
         testObject.readObject(Short.class);
         fail("Didn't throw");
      }
      catch (final StreamCorruptedException actual)
      {
         assertEquals("Missing short data", actual.getMessage());
      }

      testObject.close();
   }

   @Test
   public void readObject_allowsCast_givenNoArg()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      final byte[] fileContents = {'~', 3};  //~ is byte
      inputBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertEquals(Byte.valueOf((byte) 3), testObject.readObject());
      testObject.close();
   }

   @Test
   public void readObjectStrictly_happyPath()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      final byte[] fileContents = {
         '+', '-',  //+ is true, - is false
         '~', 3  //~ is byte
      };
      inputBuilder.append(fileContents);

      inputBuilder.append("java.math.RoundingMode;");
      inputBuilder.append(new byte[]{0, 0, 0, 1});
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
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
   public void readFieldsReflectively()
   {
      final ByteAppender inputBuilder = new ByteAppender();
      final String header = "com.github.skySpiral7.java.staticSerialization.ObjectStreamReader_UT$ReflectiveClass;@";
      inputBuilder.append(header);
      final byte[] fileContents = {0x0a, (byte) 0xfe, (byte) 0xba, (byte) 0xbe};
      inputBuilder.append(fileContents);
      final ByteReader mockFile = new ByteReader(inputBuilder.getAllBytes());

      final ObjectStreamReader testObject = new ObjectStreamReader(mockFile);
      assertEquals(0x0afe_babeL, testObject.readObject(ReflectiveClass.class).field);

      testObject.close();
   }
}
