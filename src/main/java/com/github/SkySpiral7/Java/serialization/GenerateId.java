package com.github.SkySpiral7.Java.serialization;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <p>This annotation only has meaning when placed on a class that implements StaticSerializable.
 * It is used to mark that the read and write methods should automatically generate an id
 * in order to resolve multiple references to be the same instance instead of infinite recursion/iteration.
 * This is true for both direct and indirect references (such as a circular reference).</p>
 * <p>The reason this annotation exists instead of being the default functionality is that the vast
 * majority of cases will not have a need for this. In such cases generating an unused id would create
 * useless overhead.</p>
 * <p><strong>Implementation note:</strong> in order for this annotation to function you must also call
 * {@code ObjectReaderRegistry.claimId} in the readFromStream method after the object is created but before
 * it has any data will cause back-referencing.</p>
 *
 * @see StaticSerializable
 * @see ObjectReaderRegistry#claimId(Object)
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface GenerateId
{}
