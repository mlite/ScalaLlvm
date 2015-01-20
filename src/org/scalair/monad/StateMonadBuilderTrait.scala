package org.scalair.monad

//import org.scalair.monad.MonadBuilderTrait.Monad._
//import org.scalair.monad.TransformedMonadBuilderTrait.TransformedMonad._

trait StateMonadBuilderTrait extends MonadBuilderTrait with StateBuilderTrait {
  //type M[+Z] <: StateMonad[Z]
  override type M[+Z] = StateMonad[Z]
  override type In[-Z] = S
  override type Out[+Z] = (Z,S)


  /*
  override def _returnState[A]  = { x:A => s:S =>

  }
  */

  override def _return[A] = { x:A =>
    new StateMonad[A] {
      def run[B>:A](in:In[B]):Out[B] = (x,in)
    }
  }

  override def _get:Unit => M[S] = { _:Unit =>
    new StateMonad[S] {
      def run[B>:S](in:In[S]):Out[S] = (in, in)
    }
  }

  override def _set:S => M[Unit] = { s:S =>
    new StateMonad[Unit] {
      def run [B>:Unit](in:In[B]):Out[B] = ((), s)
    }
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


  trait StateMonad[+A] extends Monad[A] with State[A] { self: M[A] =>
    override def flatMap[B](a_f_mb: A => M[B]):M[B] = {
      new StateMonad[B] {
        def run[C>:B](s:S):Out[C] = {
          self.run(s) match {
            case (a:A, s1) => a_f_mb(a).run(s1)
          }
        }
      }
    }
  }
}