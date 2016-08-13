package com.github.SkySpiral7.Java.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

/**
 * This is a simple utility for reading and writing files.
 * Simple to use but none are efficient.
 * 
 * @see #writeToFile(File, String, Charset, boolean)
 * @see #readFromFileAsString(File, Charset)
 */
public enum FileIoUtil
//I tested this class by hand since a UT would have to duplicate the code
{
	;  //no instances

	/**
	 * @param targetFile
	 *           writes to this file (clearing previous content)
	 * @param newContents
	 *           the file will contain only this UTF-8 string
	 * @throws RuntimeException
	 *            of FileNotFoundException or IOException
	 * @see #writeToFile(File, String, Charset, boolean)
	 */
	public static void writeToFile(final File targetFile, final String newContents)
	{
		writeToFile(targetFile, newContents, StandardCharsets.UTF_8, false);
	}

	/**
	 * @param targetFile
	 *           writes to this file (keeping previous content)
	 * @param newContents
	 *           the file will gain this UTF-8 string (after current contents)
	 * @throws RuntimeException
	 *            of FileNotFoundException or IOException
	 * @see #writeToFile(File, String, Charset, boolean)
	 */
	public static void appendToFile(final File targetFile, final String newContents)
	{
		writeToFile(targetFile, newContents, StandardCharsets.UTF_8, true);
	}

	/**
	 * This method opens the file, writes (which may create it), then closes the file.
	 * Therefore it is inefficient to call this more than once.
	 * 
	 * @param targetFile
	 *           writes to this file
	 * @param newContents
	 *           the string contents to be written
	 * @param encoding
	 *           the character encoding to write to the file in
	 * @param willAppend
	 *           true if the current file contents should be kept
	 * @throws RuntimeException
	 *            of FileNotFoundException or IOException
	 */
	public static void writeToFile(final File targetFile, final String newContents, final Charset encoding,
			final boolean willAppend)
	{
		if (targetFile.isDirectory()) throw new IllegalArgumentException("It is not possible to write to a directory");
		Objects.requireNonNull(newContents);
		try
		{
			// might create the file
			final Writer writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(targetFile, willAppend),
					encoding));
			writer.write(newContents);
			writer.close();
		}
		catch (final Exception e)
		{
			throw new RuntimeException(e);
		}
	}

	/**
	 * @param targetFile
	 *           the UTF-8 file to be read
	 * @return the entire file contents
	 * @throws RuntimeException
	 *            of FileNotFoundException or IOException
	 * @throws IllegalArgumentException
	 *            if the file is larger than a string can hold
	 * @see #readFromFileAsString(File, Charset)
	 */
	public static String readFromFileAsString(final File targetFile)
	{
		return readFromFileAsString(targetFile, StandardCharsets.UTF_8);
	}

	/**
	 * This method reads the entire file and loads it into a string.
	 * This is obviously bad for performance.
	 * 
	 * @param targetFile
	 *           the file to be read
	 * @param encoding
	 *           the character encoding to read the file with
	 * @return the entire file contents
	 * @throws RuntimeException
	 *            of FileNotFoundException or IOException
	 * @throws IllegalArgumentException
	 *            if the file is a directory or if the file doesn't exist
	 * @throws IllegalArgumentException
	 *            if the file is larger than a string can hold. The size is estimated however this method shouldn't be
	 *            used for files anywhere near the limit.
	 */
	public static String readFromFileAsString(final File targetFile, final Charset encoding)
	{
		if (targetFile.isDirectory()) throw new IllegalArgumentException(
				"It is not possible to read file contents of a directory");
		if (!targetFile.exists()) throw new IllegalArgumentException("File doesn't exist");

		//this is only an estimate since character size is not always 1 byte
		if (targetFile.length() > Integer.MAX_VALUE) throw new IllegalArgumentException(
				"File too large to fit into a string");
		//for completeness I could count the number of characters read and throw but it's better to have this hedge

		final StringBuilder returnValue = new StringBuilder();
		Reader reader;
		try
		{
			reader = new BufferedReader(new InputStreamReader(new FileInputStream(targetFile.getAbsolutePath()), encoding));
			while (true)
			{
				final int charRead = reader.read();
				if (-1 == charRead) break;  // if end of file
				returnValue.append((char) charRead);
			}
			reader.close();
		}
		catch (final Exception e)
		{
			throw new RuntimeException(e);
		}
		return returnValue.toString();
	}
}
