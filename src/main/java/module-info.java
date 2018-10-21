module com.github.skySpiral7.java.staticSerialization {
   requires Java;  //Automatic Module: com.github.SkySpiral7:Java
   //From Java: StringUtil.countCharOccurrences (in ArrayUtil.countArrayDimensions), FileIoUtil

   exports com.github.skySpiral7.java.staticSerialization.exception;
   //hides com.github.skySpiral7.java.staticSerialization.fileWrapper;
   //hides com.github.skySpiral7.java.staticSerialization.strategy;
   //hides com.github.skySpiral7.java.staticSerialization.util;
   exports com.github.skySpiral7.java.staticSerialization;
}
