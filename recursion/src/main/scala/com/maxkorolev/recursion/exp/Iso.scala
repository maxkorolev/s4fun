package com.maxkorolev.recursion.exp

trait Iso[A, B] {
  def from(x: A): B
  def to(x: B): A

}
