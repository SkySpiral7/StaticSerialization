package com.github.SkySpiral7.Java;

/**
 * This interface is an improvement over Cloneable in several ways.
 * <ol>
 * <li>This interface is not a marker. Rather it actually does have the method signature.
 * This means that, unlike clone, any class that implements this interface must have defined the method.</li>
 * <li>Further this means that there is no "not supported" exception to worry about. If it has the copy method then
 * it is supported. Unlike clone which has a checked exception, meaning even if you know the object supports cloning
 * you may need to wrap it in try catch anyway (it is possible to throw less in which case it wouldn't be checked).
 * In fact since clone requires calling super.clone() you are forced to include a catch block in the clone definition
 * (assuming you are throwing nothing) even though by implementing Cloneable it will never throw this exception.</li>
 * <li>The copy method is public. This allows any code to call it just as it was intended. Unlike clone which is protected
 * and therefore useless unless expanded to public (which can't be reflected by an interface).</li>
 * <li>This interface uses generics. This allows the coder to directly use the object without casting.</li>
 * <li>The copy method has no dependents. It doesn't matter what the inheritance structure looks like, Copyable can
 * still be implemented. Unlike clone where if your parent doesn't support cloning then the child class can't either.</li>
 * <li>The copy method allows any kind of object creation. Cloneable requires super.clone() and uses special
 * native code to create an object without using any constructor. This may cause some fields to not be initialized
 * and it avoids any code included in the constructors such as caching. Copyable on the other hand allows the use of
 * copy constructors, normal constructors, factories, or anything else thus allowing valid object creation.</li>
 * </ol>
 *
 * <p>From Object.clone():</p>
 * <blockquote>The class {@code Object} does not itself implement the interface
 * {@code Cloneable}, so calling the {@code clone} method on an object
 * whose class is {@code Object} will result in throwing an
 * exception at run time.</blockquote>
 * <p>So {@code new Object().clone()} throws but every class is required to call {@code super.clone()}.
 * What kind of madness is that? I get that it has the ability by checking {@code if(this instanceof Cloneable)}
 * but it doesn't make any OOP sense. While I'm at it using native code to dodge constructors
 * is also confusing.</p>
 *
 * @param <T> the class that is implementing this interface
 *
 * @see Cloneable
 * @see Object#clone()
 * @see #copy()
 */
//TODO: use copyable everywhere
public interface Copyable<T> {
    /**
     * <p>Creates and returns a copy of this object. The precise meaning
     * of "copy" may depend on the class of the object. The general
     * intent is that, for any object {@code x}, the expressions:</p>
     * <blockquote>
     * <code>x.copy() != x && x.copy().equals(x) && x.clone().getClass() == x.getClass()</code></blockquote>
     * will be true. However it is possible that one of the parent classes has defined copy but
     * the child has not in which case only the part that was copied needs to obey these expectations.
     * </p>
     * <p>
     * The object returned by this method needs to be independent
     * of this object (which is being copied).  To achieve this independence,
     * it may be necessary to modify one or more fields
     * before returning the copy. Typically, this means
     * copying any mutable objects that comprise the internal "deep structure"
     * of the object being copied and replacing the references to these
     * objects with references to the copies.  If a class contains only
     * primitive fields or references to immutable objects, then it is usually
     * the case that all fields may simply be assigned.
     * </p>
     * <p>
     * The expected use cases for this method are: to copy an object before returning it so that
     * modifications done to it will not be reflected internally, and so that an object can be copied and modified
     * without changing the original. Therefore if a class is immutable it has no need to be copied in
     * either of these cases.
     * </p>
     * <p>
     * It is not possible for singletons (including enums) to create new objects, therefore they can't implement this
     * interface. Immutable classes have little need for this interface but can be implemented if desired.
     * </p>
     * <p>
     * There are certain cases where returning the same object is acceptable. Such as maintaining a singleton
     * object apart from the other objects of the same class which can be copied. However if this interface is
     * implemented there should be cases where the object is copied. Any exceptions should be documented.
     * </p>
     *
     * @return a copy of this instance.
     * @see Copyable
     */
	public T copy();
}
