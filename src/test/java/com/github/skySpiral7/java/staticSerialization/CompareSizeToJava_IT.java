package com.github.skySpiral7.java.staticSerialization;

import com.github.skySpiral7.java.staticSerialization.strategy.generic.JavaSerializableStrategy;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import org.hamcrest.Matcher;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.BitSet;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;

public class CompareSizeToJava_IT
{
   private static final boolean enablePrint = false;

   @Test
   public void e2e_lessBytes_whenNull()
   {
      //separate test because the map doesn't allow null
      entireSizeAssert("null", null, Matchers::lessThanOrEqualTo);
   }

   /**
    * This test documents which types have better compression and prints by how much.
    */
   @Test
   public void e2e_lessBytes_whenFavorableType()
   {
      Map.ofEntries(
         new SimpleImmutableEntry<>("Boolean", true),
         new SimpleImmutableEntry<>("Byte", (byte) 1),
         new SimpleImmutableEntry<>("Short", (short) 1),
         new SimpleImmutableEntry<>("Integer", 1),
         new SimpleImmutableEntry<>("Long", (long) 1),
         new SimpleImmutableEntry<>("Float", (float) 1),
         new SimpleImmutableEntry<>("Double", (double) 1),
         new SimpleImmutableEntry<>("Character", '1'),
         //control, BMP (infinity), ascii, non-BMP (Crying Face)
         new SimpleImmutableEntry<>("normal string", "\n∞ > 😢"),
         new SimpleImmutableEntry<>("string with null", "\u0000"),
         //java ( 43) - static ( 23) =  20 bytes saved by using static for booleanArray
         //java ( 29) - static (  9) =  20 bytes saved by using static for compressed booleanArray
         //compressing saves 14 bytes
         new SimpleImmutableEntry<>("booleanArray", new boolean[]{
            false, true, false, true,
            false, false, true, true,
            false, true, true, true,
            true, true, true, true
         }),
         new SimpleImmutableEntry<>("BitSet.valueOf", BitSet.valueOf(new byte[]{
            (byte) 0xba, (byte) 0xbe
         })),
         //massive savings throws off the table format
         new SimpleImmutableEntry<>("new BitSet", new BitSet(2147483647)),
         new SimpleImmutableEntry<>("UUID", UUID.randomUUID()),
         new SimpleImmutableEntry<>("RoundingMode.HALF_EVEN", RoundingMode.HALF_EVEN)
      ).forEach((testName, data) -> entireSizeAssert(testName, data, Matchers::lessThanOrEqualTo));
   }

   /**
    * This test exists for the sake of printing so that I can see how much bigger they are and track ones that could be
    * better compressed.
    */
   @Test
   public void e2e_moreBytes_whenNotFavorableType()
   {
      Map.ofEntries(
         //using .toByteArray() saves a lot:
         //BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(3)):
         // java (211) - static ( 16) = 195 bytes saved by using static for serializable
         //BigInteger.TEN: java (203) - static (  8) = 195 bytes saved by using static for serializable
         new SimpleImmutableEntry<>("BigInteger", BigInteger.valueOf(Long.MAX_VALUE).multiply(BigInteger.valueOf(3)))
         //BigDecimal could likely store scale then unscaledValue BigInt
      ).forEach((testName, data) -> entireSizeAssert(testName, data, Matchers::greaterThanOrEqualTo));
   }

   @Test
   public void e2e_lessBytes_comparedToSpecificMethods() throws Exception
   {
      {
         final String testName = "writeBoolean";
         final boolean data = true;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(7);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeBoolean(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeByte";
         final byte data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(7);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeByte(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeShort";
         final short data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(8);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeShort(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeInt";
         final int data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(10);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeInt(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "write(int)";
         final int data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(7);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.write(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeLong";
         final long data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(14);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeLong(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeFloat";
         final float data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(10);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeFloat(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeDouble";
         final double data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(14);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeDouble(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeChar";
         final char data = 2;

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(8);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeChar(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeChars";
         final String data = "2";

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(8);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeChars(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeBytes";
         final String data = "2";

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(7);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeBytes(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "writeUTF";
         final String data = "2";

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(9);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.writeUTF(data);
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "write(small byte[])";
         final byte[] data = "hi".getBytes(StandardCharsets.UTF_8);

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         /* my current bytes:
          * =
          * 2 array length
          * h, i data
          * =4
          */
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(8);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            /* don't have a header for other binary since it's unclear if medium or big makes more sense.
             * neither are needed */
            out.write(data);
            writer.close();
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
      {
         final String testName = "write(big byte[])";
         /* the bigger the data is the better mine does
          * 256 saves 2 bytes
          * 1 << 10 saves 2 bytes
          * 1 << 11 saves 7 bytes
          * 1 << 16 saves 317 bytes
          * 1 << 18 saves 1,277 bytes
          * 1 << 20 saves 5,117 bytes
          * 1 << 22 saves 20,477 bytes
          * 1 << 23 takes 2 seconds and saves 40,957 bytes
          * 1 << 24 takes 3 seconds and saves 81,917 bytes
          * 1 << 26 takes 11 seconds and saves 327,677 bytes
          * 1 << 28 takes 39 seconds and saves 1,310,717 bytes (that's MiB!)
          */
         final byte[] data = new byte[1 << 16];

         final ByteAppender mockFile = new ByteAppender();
         final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
         writer.writeObject(data);
         writer.close();
         /* my bytes:
          * ] primitive array indicator
          * 1 dimension count
          * ~ component type
          * 0, 0, 0, 0 array length
          * data
          */
         final int staticSerializableLength = mockFile.getAllBytes().length;

         final ByteArrayOutputStream byteStream = new ByteArrayOutputStream(65860);
         try (final ObjectOutputStream out = new ObjectOutputStream(byteStream))
         {
            out.write(data);
            writer.close();
         }
         final int javaSerializableLength = byteStream.size();

         assertThat(testName, staticSerializableLength, is(lessThanOrEqualTo(javaSerializableLength)));
         printDiff(staticSerializableLength, javaSerializableLength, testName);
      }
   }

   private void entireSizeAssert(final String testName, final Serializable data, final Function<Integer, Matcher<Integer>> matchMaker)
   {
      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();
      final int staticSerializableLength = mockFile.getAllBytes().length;
      final int javaSerializableLength = JavaSerializableStrategy.javaSerialize(data).length;
      assertThat(testName, staticSerializableLength, is(matchMaker.apply(javaSerializableLength)));
      printDiff(staticSerializableLength, javaSerializableLength, testName);
   }

   private void printDiff(final int staticSerializableLength, final int javaSerializableLength, final String methodName)
   {
      if (!enablePrint) return;
      final long diffLength = javaSerializableLength - staticSerializableLength;
      System.out.printf("java (%s) - static (%s) = %s bytes saved by using static for %s%n", padLeft(javaSerializableLength),
         padLeft(staticSerializableLength), padLeft(diffLength), methodName);
   }

   private static String padLeft(final Object input)
   {
      return String.format("%1$3s", input);
   }
}
