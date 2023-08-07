module com.github.skySpiral7.java.staticSerialization {
   requires Java;  //Automatic Module: com.github.SkySpiral7:Java
   //From Java: FileIoUtil
   requires org.apache.logging.log4j;
   //requires org.apache.logging.log4j.core;  //not needed for some reason

   exports com.github.skySpiral7.java.staticSerialization.exception;
   //hides com.github.skySpiral7.java.staticSerialization.internal;
   //hides com.github.skySpiral7.java.staticSerialization.strategy;
   //hides com.github.skySpiral7.java.staticSerialization.stream;
   //hides com.github.skySpiral7.java.staticSerialization.util;
   exports com.github.skySpiral7.java.staticSerialization;
}
