package com.maxkorolev.recursion

package object exp {

  type Algebra[F[_], C] = F[C] => C
}
