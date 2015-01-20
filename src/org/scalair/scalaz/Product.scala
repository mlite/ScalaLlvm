package org.scalair.scalaz

case class Product[F1[_], F2[_], A](first: F1[A], second: F2[A]) {
  def tuple = (first, second)
}

