package com.bones.sjson

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.{Base64, UUID}

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import shapeless.{HList, Nat, ::}


trait StringOutInterpreter {

  val dateTimeFormatter: DateTimeFormatter
  val dateFormatter: DateTimeFormatter


  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL]): H => Option[String] = {
    group match {
      case KvpNil                                          => _ => None
      case op: KvpSingleValueHead[h, t, tl, a]             =>
        val valueF = valueDefinition(op.fieldDefinition.op)
        val tailF = kvpHList(op.tail)
        implicit val hCons = op.isHCons
        (input: H) =>
        {
          val val1 = valueF(input.head).map("\"" + op.fieldDefinition.key + "\":"+_)
          val tail = tailF(input.tail)
          (val1, tail) match {
            case (Some(v), Some(t)) => Some(v + "," + t)
            case (None, _) => tail
            case (_,None) => val1
          }

        }

      case op: KvpHListHead[a, al, h, hl, t, tl]           =>
        val headF = kvpHList(op.head)
        val tailF = kvpHList[t, tl](op.tail)
        (input: H) =>
        {
          val l = op.split(input)
          val headOut = headF(l._1)
          val tailOut = tailF(l._2)
          (headOut, tailOut) match {
            case (Some(h), Some(t)) => Some(h + t)
            case (None, _) => tailOut
            case (_,None) => headOut
          }
        }

      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] =>
        val headF = kvpHList(op.hListConvert.from)
        val tailF = kvpHList(op.tail)
        implicit val hCons = op.isHCons
        (input: a :: ht) =>
        {
          val head = headF(op.hListConvert.fAtoH(input.head))
          val tail = tailF(input.tail)
          (head, tail) match {
            case (Some(h), Some(t)) => Some(h + t)
            case (None, _) => tail
            case (_,None) => head
          }
        }
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): A => Option[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        val someF = valueDefinition(op.valueDefinitionOp)
        _ match {
          case Some(s) => someF(s)
          case None => None
        }
      case ob: BooleanData                => b => if (b) Some("true") else Some("false")
      case rs: StringData                 => s => Some("\"" + s + "\"")
      case ri: LongData                   => l => Some(l.toString)
      case uu: UuidData                   => u => Some("\"" + u.toString + "\"")
      case ld: LocalDateData              => d => Some("\"" + dateFormatter.format(d) + "\"")
      case dd: LocalDateTimeData          =>  d => Some("\"" + dateTimeFormatter.format(d) + "\"")
      case bd: BigDecimalData             => bd => Some(bd.toString)
      case ld: ListData[t]                =>
        l =>
          val tDef =  valueDefinition(ld.tDefinition)
          Some(l.flatMap(t => tDef(t)).mkString("[",",","]"))
      case dd: DoubleData                 => d => Some(d.toString)
      case fd: FloatData                  => f => Some(f.toString)
      case id: IntData                    => i => Some(i.toString)
      case sd: ShortData                  => s => Some(s.toString)
      case ed: EitherData[a, b]           =>
        val aDef: a => Option[String] = valueDefinition(ed.definitionA)
        val bDef: b => Option[String] = valueDefinition(ed.definitionB)
        _ match {
          case Left(l) => aDef(l)
          case Right(r) => bDef(r)
        }
      case ba: ByteArrayData              =>
        (input: Array[Byte]) => Some("\"" + Base64.getEncoder.encodeToString(input) + "\"")
      case esd: EnumerationData[e,a]  => e => Some("\"" + e.toString + "\"")
      case kvp: KvpHListValue[h, hl]      =>
        val hListDef = kvpHList(kvp.kvpHList)
        (input: A) => hListDef(input.asInstanceOf[h]).map("{" + _ + "}")
      case x: HListConvert[a, al, b]      =>
        val fromDef = kvpHList(x.from)
        (input: A) => fromDef(x.fAtoH(input)).map("{" + _ + "}")
      case s: SumTypeData[a, b]           =>
        val fh = valueDefinition(s.from)
        input: A =>
          fh(s.fba(input)).map("{" + _ + "}")
    }




}