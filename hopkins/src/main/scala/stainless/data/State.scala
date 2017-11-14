package stainless.data

import stainless.lang._
import stainless.annotation._

import scala.language.postfixOps

@library
case class State[S, A](run: S => (A, S)) { self =>

    @library
    def map[B](f: A => B): State[S, B] = State { state =>
        val (a, s) = self.run(state)
        (f(a), s)
    }

    @library
    def flatMap[B](f: A => State[S, B]): State[S, B] = State { state =>
        val (a, s) = self.run(state)
        f(a).run(s)
    }

    @library
    def exec(s: S): S =
      run(s)._2

    @library
    def eval(s: S): A =
      run(s)._1

}

object State {

    @library
    def pure[S, A](x: A): State[S, A] = {
      new State[S, A](state => (x, state))
    } ensuring { res =>
      forall((s: S) => res.eval(s) == x)
    }

    @library
    def get[S]: State[S, S] = {
      new State[S, S](state => (state, state))
    } ensuring { res =>
      forall((s: S) => res.eval(s) == res.exec(s))
    }

    @library
    def put[S](state: S): State[S, Unit] = {
      new State[S, Unit](_ => ((), state))
    } ensuring { res =>
      forall((s: S) => res.exec(s) == state)
    }

    @library
    def modify[S](f: S => S): State[S, Unit] = {
      get[S].flatMap(s => put[S](f(s)))
    } ensuring { res =>
      forall((s: S) => res.exec(s) == f(get[S].exec(s)))
    }
}

object MonadLaws {

    import State._

    @library
    def monad_leftIdentity[A, B, S](a: A, f: A => State[S, B], i: S): Boolean = {
        pure[S, A](a).flatMap(f).run(i) == f(a).run(i)
    } holds

    @library
    def monad_rightIdentity[S, A](s: State[S, A], i: S): Boolean = {
        s.flatMap(x => pure(x)).run(i) == s.run(i)
    } holds

    @library
    def monad_associativity[A, B, C, S](s: State[S, A], f: A => State[S, B], g: B => State[S, C], i: S): Boolean = {
        s.flatMap(f).flatMap(g).run(i) == s.flatMap(x => f(x).flatMap(g)).run(i)
    } holds

}

object StateLaws {

    import State._

    @library
    def state_law1[S](i: S, s: S): Boolean = {
        val state: State[S, S] = for {
          _ <- put[S](s)
          t <- get[S]
        } yield t

        state.run(i) == (s, s)
    } holds

    @library
    def state_law2[S](s: S): Boolean = {
        get[S].run(s) == (s, s)
    } holds

    @library
    def state_law3[S](a: S, b: S, i: S): Boolean = {
        val state: State[S, Unit] = for {
          _ <- put[S](a)
          _ <- put[S](b)
        } yield ()

        state.run(i) == ((), b)
    } holds

}

