package com.maxkorolev

package object recursion {

  type Algebra[F[_], C] = F[C] => C
}
