package com.github.SkySpiral7.Java.StaticSerialization;

public interface StaticSerializable
{
   //public static T<this> readFromStream(final ObjectStreamReader reader)
   public void writeToStream(final ObjectStreamWriter writer);

   //   public static <T> T readFromStream(final ObjectStreamReader reader, final Function<ObjectStreamReader, T> emptyCreate,
   //                                      final BiConsumer<ObjectStreamReader, T> populate)
   //   {
   //      final ObjectReaderRegistry registry = reader.getObjectRegistry();
   //      final T registeredObject = registry.readObjectOrId(reader);
   //      if (registeredObject != null) return registeredObject;
   //
   //      final T result = emptyCreate.apply(reader);
   //      registry.claimId(result);
   //      populate.accept(reader, result);
   //      return result;
   //   }
}
