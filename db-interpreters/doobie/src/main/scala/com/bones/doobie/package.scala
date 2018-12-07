package com.bones

import cats.effect._
import cats.implicits._
import _root_.doobie.{ConnectionIO, _}
import _root_.doobie.implicits._
import fs2.Stream

package object doobie {

  case class WithId[P](id: Long, p: P)

  trait Insertable[A] {
    type Key
    def insert(a: A): ConnectionIO[Key]
  }

  trait Findable[A] {
    type Key
    def find(k: Key): ConnectionIO[Option[A]]
  }

  trait Updatable[A] {
    type Key
    def update(k: Key, a: A): ConnectionIO[Int]
  }

  trait Deletable[A] {
    type Key
    def delete(k: Key): ConnectionIO[Int]
  }

}
