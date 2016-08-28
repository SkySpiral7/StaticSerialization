package com.github.SkySpiral7.Java;

import com.github.SkySpiral7.Java.util.FileIoUtil;
import org.junit.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class UT_AsynchronousFileAppender {

	@Test
	public void writesToFile() throws IOException {
		final File tempFile = File.createTempFile("UT_AsynchronousFileAppender.TempFile.writesToFile.", ".txt");
		tempFile.deleteOnExit();
		final AsynchronousFileAppender testObject = new AsynchronousFileAppender(tempFile);

		testObject.append("hi");
		testObject.flush();  //wait for the disk write before reading file
		assertEquals("hi", FileIoUtil.readTextFile(tempFile));
		//the test can only assert that the payload was delivered
		//using breakpoints I think I fixed all deadlocks

		testObject.close();
	}

	@Test
	public void closeTwiceDoesNothing() throws IOException {
		final File tempFile = File.createTempFile("UT_AsynchronousFileAppender.TempFile.closeTwiceDoesNothing.", ".txt");
		tempFile.deleteOnExit();
		final AsynchronousFileAppender testObject = new AsynchronousFileAppender(tempFile);
		testObject.close();
		testObject.close();
	}

	@Test
	public void appendAfterCloseThrows() throws IOException {
		final File tempFile = File.createTempFile("UT_AsynchronousFileAppender.TempFile.appendAfterCloseThrows.", ".txt");
		tempFile.deleteOnExit();
		final AsynchronousFileAppender testObject = new AsynchronousFileAppender(tempFile);
		testObject.close();
		try {
			testObject.append("hi");
		} catch (final IllegalStateException actual) {
			assertEquals("Can't write to closed stream", actual.getMessage());
		}
	}
}
