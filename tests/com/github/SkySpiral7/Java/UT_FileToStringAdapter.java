package com.github.SkySpiral7.Java;

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import org.junit.Ignore;
import org.junit.Test;

public class UT_FileToStringAdapter {
    private String projectPath = "tests/";
    //projectPath can be "" (same as "./") or any relative or absolute path. It is only used for the 2 constructors
    private String smallFileContents = "ABCDEFGHIJKLMNOPQRSTUVWXYZ\r\nABCDEFGHIJKLMNOPQRSTUVWXYZ\r\nabcdefghijklmnopqrstuvwxyz\r\nabcdefghijklmnopqrstuvwxyz";
    private FileToStringAdapter smallFile = new FileToStringAdapter(projectPath+"smallFile.txt");
    private FileToStringAdapter largeFile = new FileToStringAdapter(projectPath+"no largeFile.txt");
    //TODO: after done, turn the large file on for certain tests to find out which ones it can reasonably do
    private long largeFileLastSectionIndex = smallFileContents.length()+6+Integer.MAX_VALUE+(8*1024);
    //6 is for the 2 blank lines and first section's end line
    //Integer.MAX_VALUE is for the 0s and (8*1024) is for the end lines of the 0s

    @Test
    public void contentsAsString() {
        assertEquals(smallFileContents, smallFile.contentsAsString());
    }

    @Test
    public void isEmpty() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        assertFalse(smallFile.isEmpty());
    }

    @Test
    public void isEmpty_throw() {
    	FileToStringAdapter noContentFile = new FileToStringAdapter(".");
        assertTrue(noContentFile.isDirectory());
        try
        {
        	noContentFile.isEmpty();
        	fail("failed to throw when directory file called isEmpty");
    	}
        catch(IllegalStateException e){assertEquals("Operation is not supported for folders because they do not have contents.", e.getMessage());}

        noContentFile = new FileToStringAdapter("UT_FileToStringAdapter temporary non existent file: ?<>~!@#$%^&*()_+=`\"';,.[]{}");
        //I hope this file doesn't exist but just to be sure I'll find one that doesn't:
        while(noContentFile.exists()){noContentFile = new FileToStringAdapter(noContentFile.getAbsolutePath()+"_");}
        try
        {
        	noContentFile.isEmpty();
        	fail("failed to throw when nonexistent file called isEmpty");
    	}
        catch(IllegalStateException e){assertEquals("Operation is not supported for files that do exist because they not have contents.", e.getMessage());}
    }

    @Test
    public void charAt() {
        assertThat(smallFile.charAt(1), is('B'));

        //if(largeFile.exists()) assertThat(largeFile.charAt(largeFile.length()-4), is('y'));
        //too slow
    }

    @Test
    public void countCharacters() {
        assertEquals(smallFile.length(), smallFile.countCharacters());
        //note that this is only true because the file is ascii encoding

        if(largeFile.exists()) assertEquals(largeFile.length(), largeFile.countCharacters());
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void charAt_Throw() {
        assertThat(smallFile.length(), is(lessThan(2000L)));
        smallFile.charAt(2000);  //will throw
    }

    @Test
    public void getChars() {
        char[] testArray = new char[26];
        testArray[0]='A';
        smallFile.getChars(1, 26, testArray, 1);
        assertArrayEquals("ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray(), testArray);

        if (largeFile.exists())
        {
        	testArray = new char[26];  //clear out array
            testArray[0]='a';
            largeFile.getChars(largeFileLastSectionIndex+(2*28), largeFileLastSectionIndex+(2*28)+27, testArray, 1);
            assertArrayEquals("abcdefghijklmnopqrstuvwxyz".toCharArray(), testArray);
        }
    }

    @Test
    public void equals() {
        File smallAsFile = new File(smallFile.getAbsolutePath());
        assertTrue(smallFile.equals(smallAsFile));
        assertFalse(largeFile.equals(smallAsFile));
    }

    @Test
    public void test_hashCode() {
        File smallAsFile = new File(smallFile.getAbsolutePath());
        assertEquals(smallAsFile.hashCode(), smallFile.hashCode());
    }

    @Test
    public void test_toString() {
        File smallAsFile = new File(smallFile.getAbsolutePath());
        assertEquals(smallAsFile.toString(), smallFile.toString());
    }

    @Test
    public void contentEquals() {
        assertTrue(smallFile.contentEquals(smallFileContents));
        assertFalse(smallFile.contentEquals("AB"));

        if(largeFile.exists()) assertFalse(largeFile.contentEquals(smallFileContents));
    }

    @Test
    public void contentEqualsIgnoreCase() {
        assertTrue(smallFile.contentEqualsIgnoreCase(smallFileContents.toLowerCase()));

        if(largeFile.exists()) assertFalse(largeFile.contentEquals(smallFileContents.toLowerCase()));
    }

    @Test
    @Ignore
    public void compareContents() {
        assertEquals(0, smallFile.compareContents(smallFileContents));
        assertThat(smallFile.compareContents(smallFileContents.toUpperCase()), is(greaterThan(0)));
        assertThat(smallFile.compareContents("A"), is(greaterThan(0)));
        assertThat(smallFile.compareContents(smallFileContents+smallFileContents), is(lessThan(0)));
        assertThat(smallFile.compareContents(smallFileContents.toLowerCase()), is(lessThan(0)));

        if(largeFile.exists()) assertThat(largeFile.compareContents(smallFileContents), is(greaterThan(0)));
    }

    @Test
    @Ignore
    public void compareContentsIgnoreCase() {
        assertEquals(0, smallFile.compareContentsIgnoreCase(smallFileContents.toLowerCase()));
        assertThat(smallFile.compareContentsIgnoreCase("A"), is(greaterThan(0)));
        assertThat(smallFile.compareContentsIgnoreCase(smallFileContents+smallFileContents), is(lessThan(0)));

        if(largeFile.exists()) assertThat(largeFile.compareContentsIgnoreCase(smallFileContents), is(greaterThan(0)));
    }

    @Test
    @Ignore
    public void startsWith() {
        assertTrue(smallFile.startsWith("ABCD"));
        assertTrue(smallFile.startsWith("BCD", 1));
        assertFalse(smallFile.startsWith("abcd"));
        assertFalse(smallFile.startsWith("bcd", 1));

        if (largeFile.exists())
        {
            assertTrue(largeFile.startsWith("ABCD"));
            assertTrue(largeFile.startsWith("BCD", largeFileLastSectionIndex+1));
            assertFalse(largeFile.startsWith("abcd"));
            assertFalse(largeFile.startsWith("bcd", largeFileLastSectionIndex+1));
        }
    }

    @Test
    @Ignore
    public void endsWith() {
        assertTrue(smallFile.endsWith("wxyz"));
        assertFalse(smallFile.endsWith("WXYZ"));

        if (largeFile.exists())
        {
            assertTrue(largeFile.endsWith("wxyz\r\n"));
            assertFalse(largeFile.endsWith("WXYZ\r\n"));
        }
    }

    @Test
    @Ignore
    public void indexOf() {
        assertEquals(1, smallFile.indexOf('B'));
        assertEquals(29, smallFile.indexOf('B', 3));
        assertEquals(2, smallFile.indexOf("CD"));
        assertEquals(30, smallFile.indexOf("CD", 3));

        if (largeFile.exists())
        {
            assertEquals(1, largeFile.indexOf('B'));
            assertEquals(largeFileLastSectionIndex+29, largeFile.indexOf('B', largeFileLastSectionIndex+3));
            assertEquals(2, largeFile.indexOf("CD"));
            assertEquals(largeFileLastSectionIndex+30, largeFile.indexOf("CD", largeFileLastSectionIndex+3));
        }
    }

    @Test
    @Ignore
    public void lastIndexOf() {
        assertEquals(29, smallFile.lastIndexOf('B'));
        assertEquals(80, smallFile.lastIndexOf('y', smallFileContents.length()-5));
        assertEquals(30, smallFile.lastIndexOf("CD"));
        assertEquals(78, smallFile.lastIndexOf("wx", smallFileContents.length()-5));

        if (largeFile.exists())
        {
            assertEquals(largeFileLastSectionIndex+29, largeFile.lastIndexOf('B'));
            assertEquals(largeFileLastSectionIndex+80, largeFile.lastIndexOf('y', largeFile.length()-5));
            assertEquals(largeFileLastSectionIndex+30, largeFile.lastIndexOf("CD"));
            assertEquals(largeFileLastSectionIndex+78, largeFile.lastIndexOf("wx", largeFile.length()-5));
        }
    }

    @Test
    public void substring() {
        assertEquals("abcdefghijklmnopqrstuvwxyz", smallFile.substring(84));
        assertEquals("BCD", smallFile.substring(1, 4));

        if (largeFile.exists())
        {
            assertEquals("abcdefghijklmnopqrstuvwxyz\r\n", largeFile.substring(largeFile.length()-28));
            assertEquals("abcdef", largeFile.substring(largeFile.length()-28, largeFile.length()-22));
        }
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void contentsAsString_throw() {
        if(!largeFile.exists()) throw new IndexOutOfBoundsException();
        //this is done so that it will pass if large file doesn't exist
        largeFile.contentsAsString();
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void substring_beginIndex_throw_negative() {
        smallFile.substring(-1);
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void substring_beginIndex_throw_largerThanFile() {
        assertThat(smallFile.length(), is(lessThan(2000L)));
        smallFile.substring(2000);
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void substring_endIndex_throw_negative() {
        smallFile.substring(0, -1);
        //this also tests if beginIndex > endIndex. there isn't a way to separate these 2 tests
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void substring_endIndex_throw_largerThanFile() {
        assertThat(smallFile.length(), is(lessThan(2000L)));
        smallFile.substring(0, 2000);
    }

    @Test
    public void concat() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        tempFile.concat("123");
        tempFile.concat("456");
        assertEquals("123456", tempFile.contentsAsString());
    }

    @Test
    public void setFileContents() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        tempFile.setFileContents("123");
        assertEquals("123", tempFile.contentsAsString());
        tempFile.concat("abc");
        tempFile.setFileContents("456");
        assertEquals("456", tempFile.contentsAsString());
    }

    @Test
    @Ignore
    public void contains() {
        assertTrue(smallFile.contains("bcd"));
        assertFalse(smallFile.contains("bcD"));

        if (largeFile.exists())
        {
            assertTrue(largeFile.contains("bcd"));
            assertFalse(largeFile.contains("bcD"));
        }
    }

    @Test
    @Ignore
    public void replace() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        tempFile.concat("1232");
        tempFile.replaceAll('2', '5');
        assertEquals("1535", tempFile.contentsAsString());
        tempFile.concat("53");
        tempFile.replaceAll("53", "");
        assertEquals("15", tempFile.contentsAsString());
        tempFile.concat("1");
        tempFile.replaceFirst("1", "2");
        assertEquals("251", tempFile.contentsAsString());
        tempFile.concat("1");
        tempFile.replaceFirst('1', '3');
        assertEquals("2531", tempFile.contentsAsString());
    }

    @Test
    @Ignore
    public void split() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        tempFile.concat("1||23||4");
        assertArrayEquals(new String[]{"1", "23", "4"}, tempFile.split("||"));
        assertArrayEquals(new String[]{"1", "23||4"}, tempFile.split("||", 1));
    }

    @Test
    @Ignore
    public void toLowerCase() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        tempFile.concat("AbCD");
        tempFile.toLowerCase();
        assertEquals("abcd", tempFile.contentsAsString());
    }

    @Test
    @Ignore
    public void toUpperCase() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        tempFile.concat("aBcd");
        tempFile.toUpperCase();
        assertEquals("ABCD", tempFile.contentsAsString());
    }

    @Test
    @Ignore
    public void trim() throws IOException {
        FileToStringAdapter tempFile = new FileToStringAdapter(File.createTempFile("FileToStringAdapterTempFile", ".txt"));
        tempFile.deleteOnExit();
        assertTrue(tempFile.isEmpty());
        tempFile.concat("123       \r\n   ");
        tempFile.trimFileContents();
        assertEquals("123", tempFile.contentsAsString());
        tempFile.setFileContents("\t123  \r\n 456   \r\n\r\n");
        tempFile.trimEachLine();
        assertEquals("123\r\n456\r\n\r\n", tempFile.contentsAsString());
        tempFile.setFileContents("\t123  \r\n 456   ");
        tempFile.trimLinesTrailing();
        assertEquals("\t123\r\n 456", tempFile.contentsAsString());
        tempFile.setFileContents(" 123  \r\n\r\n\r\n 456  ");
        tempFile.removeRedundantBlankLines();
        assertEquals(" 123  \r\n\r\n 456  ", tempFile.contentsAsString());
        tempFile.concat("\r\n\r\n");
        tempFile.removeAllBlankLines();
        assertEquals(" 123  \r\n 456  \r\n", tempFile.contentsAsString());
    }

    public void createLargeFile() throws IOException {
        if(!largeFile.exists()) largeFile.createNewFile();
        char[] fillerArray = new char[1024*1024];  //1 MB of RAM will be needed
        Arrays.fill(fillerArray, '0');
        String fillerString = new String(fillerArray);  //another MB of RAM needed for a total of 2 MB
        //uses windows line encoding. WARNING: do NOT change the line encoding as it cause the tests to fail
        largeFile.setFileContents(smallFileContents+"\r\n\r\n");
        for(int i=0; i < (4*1024); i++){largeFile.concat(fillerString+"\r\n");}
        largeFile.concat("\r\n"+smallFileContents+"\r\n");
        //largeFile will be slightly bigger than 4 GB
        assertTrue(largeFile.length() > Integer.MAX_VALUE);
        //I forgot that all numbers are signed in java. so the int.max is 2 GB
        //however I want largeFile to be more than 4 GB anyway since that is a well known problematic size
        //to be exact the file contents have a size of: 4,294,975,716 bytes
        //or 4 GB, 8 KB, and 228 bytes
    }

/*largeFile looks like this:
smallFileContents

<1 MB of 0s>
<1 MB of 0s>
<1 MB of 0s>
<1 MB of 0s>
...
<1 MB of 0s>

smallFileContents
*/

    public void createMediumFile() throws IOException {
    	FileToStringAdapter mediumFile = new FileToStringAdapter("./mediumFile.txt");
        if(!mediumFile.exists()) mediumFile.createNewFile();
        char[] fillerArray = new char[1024*1024];  //1 MB of RAM will be needed
        Arrays.fill(fillerArray, '0');
        String fillerString = new String(fillerArray);  //another MB of RAM needed for a total of 2 MB
        mediumFile.setFileContents(smallFileContents+"\r\n\r\n");
        mediumFile.concat(fillerString+"\r\n");
        mediumFile.concat("\r\n"+smallFileContents+"\r\n");
        System.out.println(mediumFile.getAbsolutePath());
    }

/*mediumFile looks like this:
smallFileContents

<1 MB of 0s>

smallFileContents
*/

}
