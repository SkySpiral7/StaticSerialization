package com.github.skySpiral7.java.staticSerialization.testClasses;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;

import java.util.EnumSet;

/**
 * This example is to show that a proxy (like {@link EnumSet} has) is possible.
 * Extending isn't required for this but it's a logical use case.
 */
public abstract class AbstractParent implements StaticSerializable
{
    public static AbstractParent readFromStream(final ObjectStreamReader reader)
    {
        //bool: is mutable
        if (reader.readObject(boolean.class)) return new ChildMutable(reader.readObject(String.class));
        return new ChildImmutable(reader.readObject(int.class));
    }
}
