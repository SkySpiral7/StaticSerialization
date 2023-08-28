module com.github.skySpiral7.java.staticSerialization {
   requires org.apache.logging.log4j;

   exports com.github.skySpiral7.java.staticSerialization.exception;
   //hides com.github.skySpiral7.java.staticSerialization.internal;
   //hides com.github.skySpiral7.java.staticSerialization.strategy;
   exports com.github.skySpiral7.java.staticSerialization.stream;
   //hides com.github.skySpiral7.java.staticSerialization.util;
   exports com.github.skySpiral7.java.staticSerialization;
}
