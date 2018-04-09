package org

import cats.data._

package object validation {

  type Validation[T] = ValidatedNel[Err, T]

}
