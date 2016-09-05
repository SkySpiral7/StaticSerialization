package com.github.SkySpiral7.Java.pojo;

public enum Comparison
{
   GREATER_THAN(">"), LESS_THAN("<"), EQUAL_TO("=="), GREATER_THAN_OR_EQUAL_TO(">="), LESS_THAN_OR_EQUAL_TO("<="), NOT_EQUAL("!=");

   private final String toStringValue;

   private Comparison(String toStringValue) {this.toStringValue = toStringValue;}

   @Override
   public String toString() {return toStringValue;}
}
