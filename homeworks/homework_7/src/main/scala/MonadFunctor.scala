
trait Monad[F[_]] {

    def pure[A](a: A): F[A]

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => flatMap(fb)(b => pure(f(a, b))))

    def sequence[A](fas: List[F[A]]): F[List[A]] = fas.foldRight(pure(List[A]()))((fa, fl) => map2(fa, fl)(_ :: _))

    def compose[A, B, C](f: A => F[B])(g: B => F[C]): A => F[C] = (a: A) => flatMap(f(a))(g)
}

trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
    def functorFromMonad[F[_]](M: Monad[F]): Functor[F] = new Functor[F] {
        def map[A, B](a: F[A])(f: A => B): F[B] = M.flatMap(a)(content => M.pure(f(content)))
    }
}
