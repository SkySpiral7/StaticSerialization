package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.exception.NotSerializableException;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.JavaSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.strategy.generic.StringSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.util.BitWiseUtil;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.math.BigInteger;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

public class InternalStreamWriter_UT
{
   private final BitWiseUtil bitWiseUtil = new BitWiseUtil();

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
   public void writeObject_custom()
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

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
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
   public void writeObject_customEnum()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(CustomEnum.class.getName());
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append("*");
      expectedBuilder.append(new byte[]{0, 0, 0, 3});  //UTF-8 length (int)
      expectedBuilder.append("One");
      final byte[] expected = expectedBuilder.getAllBytes();

      testObject.writeObject(CustomEnum.One);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_Serializable()
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);

      final BigInteger data = BigInteger.TEN;
      final byte[] javaData = JavaSerializableStrategy.javaSerialize(data);
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append("java.math.BigInteger");
      expectedBuilder.append(StringSerializableStrategy.TERMINATOR);
      expectedBuilder.append(bitWiseUtil.toBigEndianBytes(javaData.length, 4));
      expectedBuilder.append(javaData);
      final byte[] expected = expectedBuilder.getAllBytes();

      testObject.writeObject(data);
      testObject.close();
      final byte[] fileContents = mockFile.getAllBytes();
      assertEquals(Arrays.toString(expected), Arrays.toString(fileContents));
   }

   @Test
   public void writeObject_throw_unknownClass()
   {
      final ByteAppender mockFile = new ByteAppender();

      final ObjectStreamWriter testObject = new ObjectStreamWriter(mockFile);
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
}
