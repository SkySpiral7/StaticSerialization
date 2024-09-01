package com.github.skySpiral7.java.staticSerialization.strategy.generic;

import com.github.skySpiral7.java.staticSerialization.ObjectStreamReader;
import com.github.skySpiral7.java.staticSerialization.ObjectStreamWriter;
import com.github.skySpiral7.java.staticSerialization.StaticSerializable;
import com.github.skySpiral7.java.staticSerialization.stream.ByteAppender;
import com.github.skySpiral7.java.staticSerialization.stream.ByteReader;
import org.junit.jupiter.api.Test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

class StaticSerializableStrategyTest
{
   @Test
   public void e2e_staticEnum()
   {
      final SemiMutableEnum data = SemiMutableEnum.MutableInstance;
      data.setFieldData(10);

      final ByteAppender mockFile = new ByteAppender();
      final ObjectStreamWriter writer = new ObjectStreamWriter(mockFile);
      writer.writeObject(data);
      writer.close();

      final byte[] fileBytes = mockFile.getAllBytes();
      final ByteAppender expectedBuilder = new ByteAppender();
      expectedBuilder.append(SemiMutableEnum.class.getName());
      expectedBuilder.append(new byte[]{
         StringSerializableStrategy.TERMINATOR,
         (byte) '@',  //integer type
         0, 0, 0, 0,  //ordinal
         (byte) '@',  //integer type
         0, 0, 0, 10  //field data
      });
      assertThat(fileBytes, is(expectedBuilder.getAllBytes()));

      data.setFieldData(100);
      assertEquals(100, data.getFieldData());

      final ObjectStreamReader reader = new ObjectStreamReader(new ByteReader(fileBytes));
      assertSame(SemiMutableEnum.MutableInstance, reader.readObject(SemiMutableEnum.class));
      assertEquals(10, data.getFieldData());
      reader.close();
   }

   public static enum SemiMutableEnum implements StaticSerializable
   {
      MutableInstance(1), ImmutableInstance(2);
      private int fieldData;

      SemiMutableEnum(int fieldData){this.fieldData = fieldData;}

      public int getFieldData()
      {
         return fieldData;
      }

      public void setFieldData(int fieldData)
      {
         if (this == MutableInstance) this.fieldData = fieldData;
      }

      @Override
      public void writeToStream(ObjectStreamWriter writer)
      {
         //manually write ordinal since the enum strat isn't used
         writer.writeObject(this.ordinal());

         if (this == MutableInstance) writer.writeObject(fieldData);
         //else do nothing
      }

      public static SemiMutableEnum readFromStream(final ObjectStreamReader reader)
      {
         //manually read ordinal since the enum strat isn't used
         Integer ordinal = reader.readObject(Integer.class);
         SemiMutableEnum semiMutableEnum = SemiMutableEnum.class.getEnumConstants()[ordinal];

         if (semiMutableEnum == MutableInstance) semiMutableEnum.setFieldData(reader.readObject(int.class));
         //else do nothing

         return semiMutableEnum;
      }
   }
}
