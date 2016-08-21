package com.github.SkySpiral7.Java.util;

public enum ClassUtil
{
	;  //no instances

	/**
	 * <p>
	 * This method will do an unchecked cast to T (the captured return type). The idea is that calling this method allows
	 * you to avoid raw types and unchecked casting warnings. Since T will be erased, calling this method won't throw. If
	 * the return type of this method is ambiguous, it may take the first choice instead of the desired class. And since
	 * overloading is determined at compile time you should double check that you got what you intended.
	 * </p>
	 * <p>
	 * If you have a {@code Class<T> clazz} then instead call clazz.cast which is unchecked and might throw (which is
	 * good). clazz.cast also avoids ambiguities mentioned above.
	 * </p>
	 * 
	 * @param anything
	 *           will cast this to T
	 * @return anything as T
	 */
	@SuppressWarnings("unchecked")
	public static <T> T cast(final Object anything)
	{
		return (T) anything;
	};

}
