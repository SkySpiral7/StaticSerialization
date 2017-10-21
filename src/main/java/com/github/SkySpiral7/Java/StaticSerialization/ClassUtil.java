package com.github.SkySpiral7.Java.StaticSerialization;

/**
 * Copied from Java repo to lessen dependency.
 *
 * @see com.github.SkySpiral7.Java.util.ClassUtil
 */
enum ClassUtil
{
   ;  //no instances

   /**
    * Copied from Java repo to lessen dependency.
    *
    * @see com.github.SkySpiral7.Java.util.ClassUtil#cast(Object)
    */
   @SuppressWarnings("unchecked")
   static <T> T cast(final Object anything)
   {
      return (T) anything;
   }
}