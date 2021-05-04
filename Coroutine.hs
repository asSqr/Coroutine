{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad (liftM, liftM2, ap)
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor.Identity (Identity (..))

newtype Coroutine s m r = Coroutine {
  resume :: m (Either (s (Coroutine s m r)) r)
}

instance (Functor s, Monad m) => Functor (Coroutine s m) where
  -- fmap :: (a -> b) -> Coroutine m a -> Coroutine m b
  fmap = liftM

instance (Functor s, Monad m) => Applicative (Coroutine s m) where
  pure = Coroutine . return . Right
  (<*>) = ap

instance (Functor s, Monad m) => Monad (Coroutine s m) where
  return x = Coroutine (return (Right x))
  t >>= f = Coroutine (resume t >>= either (return . Left . fmap (>>=f)) (resume . f))

instance Functor s => MonadTrans (Coroutine s) where
  lift = Coroutine . liftM Right

suspend :: (Functor s, Monad m) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (return (Left s))

type Trampoline m x = Coroutine Identity m x
type Generator a m x = Coroutine ((,) a) m x
type Iteratee a m x = Coroutine ((->) a) m x

pause :: Monad m => Trampoline m ()
pause = suspend (Identity $ return ())

yield :: (Monad m, Functor ((,) x)) => x -> Generator x m ()
yield x = suspend (x, return ())

await :: (Monad m, Functor ((->) x)) => Iteratee x m x
await = suspend return

run :: Monad m => Trampoline m x -> m x
run t = resume t >>= either (run . runIdentity) return

runGenerator :: Monad m => Generator x m r -> m ([x], r)
runGenerator = run' id where
  run' f g = resume g >>= either (\(x, cont) -> run' (f . (x:)) cont) (\r -> return (f[], r))

runIteratee :: Monad m => [x] -> Iteratee x m r -> m r
runIteratee (x:rest) i = resume i >>= either (\cont -> runIteratee rest (cont x)) return
runIteratee [] i = resume i >>= either (\cont -> runIteratee [] (cont $ error "No more values to feed.")) return

hello = do
  lift (putStr "Hello, ")
  pause
  lift (putStrLn "World!")

gen = do
  lift (putStr "Yielding one, ")
  yield 1
  lift (putStr "then two, ")
  yield 2
  lift (putStr "returning three: ")
  yield 3

iter = do
  lift (putStr "Enter two numbers: ")
  a <- await
  b <- await
  lift (putStrLn ("sum is " ++ show (a+b)))

newtype Compose f g a = Compose { getCompose :: f (g a) }
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

data EitherFunctor l r x = LeftF (l x) | RightF (r x)
instance (Functor l, Functor r) => Functor (EitherFunctor l r) where
  fmap f (LeftF l) = LeftF (fmap f l)
  fmap f (RightF r) = RightF (fmap f r)

bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = do
  a <- ma
  b <- mb
  f a b

type Transducer a b m x = Coroutine (EitherFunctor ((->) (Maybe a)) ((,) b)) m x

awaitT :: Monad m => Transducer a b m (Maybe a)
awaitT = suspend (LeftF return)

yieldT :: Monad m => b -> Transducer a b m ()
yieldT x = suspend (RightF (x, return ()))

lift121 :: Monad m => (a -> b) -> Transducer a b m ()
lift121 f = awaitT >>= maybe (return ()) (\a -> yieldT (f a) >> lift121 f)

liftStateless :: Monad m => (a -> [b]) -> Transducer a b m ()
liftStateless f = awaitT >>= maybe (return ()) (\a -> mapM_ yieldT (f a) >> liftStateless f)

liftStateful :: Monad m => (state -> a -> (state, [b])) -> (state -> [b]) -> state -> Transducer a b m ()
liftStateful f eof s = awaitT >>= maybe (mapM_ yieldT (eof s)) (\a -> let (s', bs) = f s a in mapM_ yieldT bs >> liftStateful f eof s')

(=>=) :: Monad m => Transducer a b m x -> Transducer b c m y -> Transducer a c m (x,y)
