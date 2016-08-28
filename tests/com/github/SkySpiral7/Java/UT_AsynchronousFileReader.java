package com.github.SkySpiral7.Java;

import com.github.SkySpiral7.Java.util.FileIoUtil;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class UT_AsynchronousFileReader {

	@Test
	public void readsFromFile() throws IOException {
		final File tempFile = File.createTempFile("UT_AsynchronousFileReader.TempFile.readsFromFile.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "hi");
		final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);

		assertEquals("hi", testObject.readBytesAsString(2));
		//the test can only assert that the payload was delivered
		//using breakpoints I think I fixed all deadlocks

		testObject.close();
	}

	@Test
	public void closeTwiceDoesNothing() throws IOException {
		final File tempFile = File.createTempFile("UT_AsynchronousFileReader.TempFile.closeTwiceDoesNothing.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "hi");  //make sure it ignores remaining data
		final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
		testObject.close();
		testObject.close();
	}

	@Test
	public void readAfterCloseThrows() throws IOException {
		final File tempFile = File.createTempFile("UT_AsynchronousFileReader.TempFile.readAfterCloseThrows.", ".txt");
		tempFile.deleteOnExit();
		final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
		testObject.close();
		try {
			testObject.readBytes(1);
		} catch (final IllegalStateException actual) {
			assertEquals("Can't read from a closed stream", actual.getMessage());
		}
	}

	@Test
	public void readEndOfFileThrows() throws IOException {
		final File tempFile = File.createTempFile("UT_AsynchronousFileReader.TempFile.readEndOfFileThrows.", ".txt");
		tempFile.deleteOnExit();
		FileIoUtil.writeToFile(tempFile, "hi");
		final AsynchronousFileReader testObject = new AsynchronousFileReader(tempFile);
		try {
			testObject.readBytes(3);
		} catch (final IllegalStateException actual) {
			assertEquals("expected 3 bytes, found 2 bytes", actual.getMessage());
		}
		testObject.close();
	}
}
