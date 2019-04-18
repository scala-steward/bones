package com.bones.doobie

import java.sql.{Connection, PreparedStatement, SQLException, Types}
import java.time.ZonedDateTime
import java.util.UUID

import com.bones.data.Value._
import shapeless.{HList, HNil, Nat}
import DoobieUtil._

object DbInsertValues {

  type FieldName = String
  type FieldValue = String
  type Key = String
  type ColumnName = String
  type SetValue = PreparedStatement => Unit
  type SetNull = PreparedStatement => Unit
  type Index = Int
  type ID = Long
  type InsertPair[A] = Key => (Index, A) => (Index, List[(ColumnName, SetValue)])

  case class DbException(sQLException: SQLException, query: Option[String])

//  case class InsertResult[A](A => List[(ColumnName, SetValue)])


  def insertQuery[A](bonesSchema: BonesSchema[A]): A => Connection => Either[DbException, A] =
    bonesSchema match {
      case x: XMapData[h,n,b] => {
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val updates = valueDefinition(x)
        a: A => {
          val result = updates("")(1,a)
          val sql = s"""insert into $tableName ( ${result._2.map(_._1).mkString(",")} ) values ( ${result._2.map(_ => "?").mkString(",")}  )"""
          con: Connection => {
            val statement = con.prepareCall(sql)
            try {
              result._2.map(_._2).foreach(f => f(statement))
              statement.execute()
              Right(a)
            } catch {
              case e: SQLException => Left(DbException(e,Some(sql)))
            } finally {
              statement.close()
            }
          }
        }
      }
    }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): (Index, H) => (Index, List[(ColumnName, SetValue)]) = {
    group match {
      case KvpNil => (i,h) => (i, List.empty)
      case op: KvpSingleValueHead[h, t, tl, a] => {
        val headF = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val tailF = kvpGroup(op.tail)
        (i: Index,h:H) => {
          val headResult = headF(i,h.head)
          val tailResult = tailF(headResult._1,h.tail)
          (tailResult._1, headResult._2 ::: tailResult._2)
        }
      }
      case op: KvpGroupHead[a, al, h, hl, t, tl] => {
        val headF = kvpGroup(op.head)
        val tailF = kvpGroup(op.tail)
        (i:Index,h:H) => {
          val hSplit = op.split(h)
          val headList = headF(i,hSplit._1)
          val tailList = tailF(headList._1, hSplit._2)
          (tailList._1, headList._2 ::: tailList._2)
        }
      }
      case op: OptionalKvpGroup[h,hl] => ???
    }
  }

  /** Create the return type for valueDefinition given the arguments */
  private def psF[A](f: (PreparedStatement,  Index, A) => Unit) : InsertPair[A] =
    key => {
      val columnName = camelToSnake(key)
      (index: Index, a: A) => {
        val setValue: SetValue = ps => {
          f(ps, index, a)
        }
        (index + 1, List( (columnName, setValue) ))
      }
    }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): InsertPair[A] =
    fgo match {
      case op: OptionalValueDefinition[b] =>
        val valueF = valueDefinition(op.valueDefinitionOp)
        key => {
          (index: Index, a: A) => {
            a match {
              case Some(b) => valueF(key)(index,b)
              case None => (index, List.empty)
            }
          }
        }
      case ob: BooleanData =>
        psF[Boolean]( (ps,i,a) => ps.setBoolean(i,a))
      case rs: StringData =>
        psF[String]( (ps,i,a) => ps.setString(i,a))
      case ri: LongData =>
        psF[Long]( (ps,i,a) => ps.setLong(i,a))
      case uu: UuidData =>
        psF[UUID]( (ps,i,a) => ps.setString(i,a.toString))
      case dd: DateTimeData =>
        psF[ZonedDateTime]( (ps, i ,a) => ps.setDate(i, new java.sql.Date(a.toInstant.toEpochMilli)))
      case bd: BigDecimalData =>
        psF[BigDecimal]( (ps,i, a) => ps.setBigDecimal(i,a.underlying))
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] =>
        psF[A]( (ps,i,a) => ps.setString(i,a.toString))
      case esd: EnumStringData[a] =>
        psF( (ps,i,a) => ps.setString(i,a.toString) )
      case kvp: KvpGroupData[h,hl] =>
        val groupF = kvpGroup(kvp.kvpGroup)
        k => {
          (index, a) => {
            groupF(index,a.asInstanceOf[h])
          }
        }
      case x: XMapData[a,al,b] =>
        val groupF = kvpGroup(x.from)
        k => {
          (index, h) => {
            groupF(index,x.fba(h))
          }
        }
      case s: SumTypeData[a,b] =>
        val groupF = valueDefinition(s.from)
        k => {
          val groupK = groupF(k)
          (index, a) => {
            groupK(index,s.fba(a))
          }
        }
    }

}