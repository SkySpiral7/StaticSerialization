package com.github.skySpiral7.java.staticSerialization.strategy;

import com.github.skySpiral7.java.staticSerialization.exception.StreamCorruptedException;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamReader;
import com.github.skySpiral7.java.staticSerialization.internal.InternalStreamWriter;
import com.github.skySpiral7.java.staticSerialization.stream.EasyReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

public enum StringSerializableStrategy
{
   ;  //no instances
   private static final Logger LOG = LogManager.getLogger();

   public static void writeWithLength(final InternalStreamWriter internalStreamWriter, final String data)
   {
      LOG.debug(data);
      final byte[] writeMe = data.getBytes(StandardCharsets.UTF_8);
      internalStreamWriter.getStrategyInstances().getIntegerSerializableStrategy().write(writeMe.length);
      internalStreamWriter.getAppender().append(writeMe);
   }

   public static String readWithLength(final InternalStreamReader internalStreamReader)
   {
      final EasyReader reader = internalStreamReader.getReader();
      final int stringByteLength =
         internalStreamReader.getStrategyInstances().getIntegerSerializableStrategy().read("Missing string byte length");
      /*TODO: could use an Overlong null delimiter 0xC080 to reduce overhead by 2 but harder to read stream
      could also make a new string type for null delimited 0x00 (only used when contains no null)
      AsynchronousFileReader would need a readBytesUntil*/
      byte[] stringData = StreamCorruptedException.throwIfNotEnoughData(reader, stringByteLength, "Missing string data");
      final String result = new String(stringData, StandardCharsets.UTF_8);
      LOG.debug(result);
      return result;
   }

   public static void writeClassName(final InternalStreamWriter internalStreamWriter, final String className)
   {
      LOG.debug(className);
      //can't use recursion to write the string because that's endless and needs different format
      final byte[] writeMe = className.getBytes(StandardCharsets.UTF_8);
      internalStreamWriter.getAppender().append(writeMe);
      internalStreamWriter.getStrategyInstances().getByteSerializableStrategy().writeByte(';');
      //instead of size then string have the string terminated by ; since this saves 3 bytes and class names can't contain ;
   }

   public static String readClassName(final EasyReader reader, final byte firstByte)
   {
      final ByteArrayOutputStream classNameStream = new ByteArrayOutputStream();
      classNameStream.write(firstByte);
      while (true)
      {
         final byte thisByte = StreamCorruptedException.throwIfNotEnoughData(reader, 1, "Incomplete header: class name not terminated")[0];
         if (thisByte == ';') break;
         classNameStream.write(thisByte);
      }
      final String result = classNameStream.toString(StandardCharsets.UTF_8);
      LOG.debug(result);
      return result;
   }
}
