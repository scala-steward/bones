package com.bones.jdbc.column

import com.bones.data.values._

trait ScalaCoreDbColumnValue extends ColumnValue[ScalaCoreValue] {
  override def toColumns[A](alg: ScalaCoreValue[A]): ToColumns =
    alg match {
      case ob: BooleanData            => nameToColumn("bool")
      case rs: StringData             => nameToColumn("text")
      case i: ShortData               => nameToColumn("int2")
      case i: IntData                 => nameToColumn("integer")
      case ri: LongData               => nameToColumn("int8")
      case fd: FloatData              => nameToColumn("real")
      case dd: DoubleData             => nameToColumn("double precision")
      case bd: BigDecimalData         => nameToColumn("numeric")
      case bd: ByteArrayData          => nameToColumn("bytea")
      case esd: EnumerationData[e, a] => nameToColumn("text")

    }
}
