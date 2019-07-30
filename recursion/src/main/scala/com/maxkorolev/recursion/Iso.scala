package com.maxkorolev.recursion

trait Iso[A, B] {
  def from(x: A): B
  def to(x: B): A

}
