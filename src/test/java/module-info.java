open module com.github.skySpiral7.java.staticSerialization {
   //copy of prod require:
   requires org.apache.logging.log4j;

   //test only:
   requires com.github.skySpiral7.java;  //com.github.SkySpiral7:Java
   //From Java: FileIoUtil
   requires hamcrest.all;
   requires org.junit.jupiter.api;
   requires org.junit.jupiter.engine;
   //requires org.apache.logging.log4j.core;  //not needed for some reason
}
