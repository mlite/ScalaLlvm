package org.scalair.monad

trait StateTmonadBuilderTrait extends MonadBuilderTrait with StateBuilderTrait with TransformedMonadBuilderTrait {
  override type M[+Z] = StateTmonad[Z]
  override type ToM[+Z] = S => FromM[(Z, S)]
  override type In[-Z] = S
  override type Out[+Z] = FromM[(Z, S)]

  def _closeT[Z]:ToM[Z] => M[Z] = {f:ToM[Z] =>
    new StateTmonad[Z] {
      def openT = f
      override def run[B >: Z](i:In[B]):Out[B] = f(i)
    }
  }

  // FromM to ToM
  override def _lift[Z] =
    mz => s =>
      mz flatMap { z =>
        _fromReturn((z, s))
      }

  override def _get =
    _ =>
      _closeT { s =>
        _fromReturn((s, s))
      }

  override def _set =
    s =>
      _closeT { _ =>
        _fromReturn(((), s))
      }

  // update the current state with
  // the function 'f' and set value
  // as unit
  override def _modify: (S => S) => M[Unit] =
    f =>
      for {
        s <- _get(())
        u <- _set(f(s))
      } yield u

  trait StateTmonad[+A] extends Monad[A] with State[A] with TransformedMonad[A] { self: M[A] =>
    override def flatMap[B](a_f_mb: A => M[B]) =
      _closeT { s =>
        openT(s) flatMap { case (a, s1) => a_f_mb(a) openT s1 }
      }

    final def runState[B >: A](s: S)(i: FromIn[(B, S)]): FromOut[(B, S)] =
      this openT s run i

    def run[B >: A](i: In[B]): Out[B] = throw new Exception()

    override def toString = "StateTmonad[" + this.openT + "]"
  }
}