package com.github.SkySpiral7.Java.dataStructures;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.hamcrest.collection.IsIterableContainingInOrder;
import org.junit.Before;
import org.junit.Test;

public class UT_LinkedList {
    private LinkedList<String> linkedList;

    @Before
    public void setUp() {
    	linkedList = new LinkedList<>();
    }

    @Test
    public void add() {
    	assertTrue(linkedList.isEmpty());
    	linkedList.add("A");
    	assertEquals("A", linkedList.last.getData());
    	linkedList.add("B");
    	assertEquals("B", linkedList.last.getData());
    	linkedList.add("C");
    	assertEquals("C", linkedList.last.getData());
    	assertEquals("A", linkedList.first.getData());
    	assertThat(linkedList, IsIterableContainingInOrder.contains("A", "B", "C"));
    }

    @Test
    public void add_int() {
    	assertTrue(linkedList.isEmpty());
    	linkedList.add("A");
    	linkedList.add(0, "B");
    	linkedList.add(1, "C");
    	assertThat(linkedList, IsIterableContainingInOrder.contains("B", "C", "A"));
    }

    @Test
    public void addAll() {
    	assertTrue(linkedList.isEmpty());
    	linkedList.addAll(Arrays.asList("A", "B", "C"));
    	assertThat(linkedList, IsIterableContainingInOrder.contains("A", "B", "C"));
    }

    @Test
    public void clear() {
    	assertTrue(linkedList.isEmpty());
    	linkedList.add("A");
    	linkedList.addFirst("B");
    	linkedList.addLast("C");
    	assertFalse(linkedList.isEmpty());
    	linkedList.clear();
    	assertTrue(linkedList.isEmpty());
    }
    //http://courses.cs.washington.edu/courses/cse332/12sp/section/week2/QueueTester.java
    //make a test for inserting in random locations

    @Test
    public void isEmpty() {
    	assertNull(linkedList.last);
    	assertNull(linkedList.first);
    	assertTrue(linkedList.isEmpty());
    	linkedList.add("A");
    	assertFalse(linkedList.isEmpty());
    }

    @Test
    public void size() {
    	assertTrue(linkedList.isEmpty());
    	assertEquals(0, linkedList.size);
    	assertEquals(0, linkedList.size());
    	linkedList.add("A");
    	assertEquals(1, linkedList.size());
    	linkedList.add("B");
    	assertEquals(2, linkedList.size());
    	linkedList.remove(0);
    	assertEquals(1, linkedList.size());
    }

}
