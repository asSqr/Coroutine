{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}
import Control.Monad (liftM, liftM2)
import Control.Monad.Trans (MonadTrans (..))

newtype Trampoline m r = Trampoline {
  bounce :: m (Either (Trampoline m r) r)
}

instance Monad m => Functor (Trampoline m) where
  -- fmap :: (a -> b) -> Trampoline m a -> Trampoline m b
  fmap f t = Trampoline (bounce t >>= either (return . Left . (fmap f)) (return . Right . f))

instance Monad m => Applicative (Trampoline m) where
  pure = Trampoline . return . Right
  -- (<*>) :: Trampoline m (a -> b) -> Trampoline m a -> Trampoline m b
  f <*> a = Trampoline (bounce a >>= either (return . Left . (f <*>)) (\a' -> run f >>= \f' -> return (Right (f' a'))))

instance Monad m => Monad (Trampoline m) where
  return = Trampoline . return . Right
  t >>= f = Trampoline (bounce t >>= either (return . Left . (>>=f)) (bounce . f))

instance MonadTrans Trampoline where
  lift = Trampoline . liftM Right

pause :: Monad m => Trampoline m ()
pause = Trampoline (return $ Left $ return ())

run :: Monad m => Trampoline m r -> m r
run t = bounce t >>= either run return

{-
  Left (Right ())

  bounce t >>= \_ -> Left () >>= Left (() >>= Right ())
-}

hello = do
  lift (putStr "Hello, ")
  pause
  lift (putStrLn "World!")

mzipWith :: Monad m => (a -> b -> c) -> Trampoline m a -> Trampoline m b -> Trampoline m c
mzipWith f t1 t2 = Trampoline (liftM2 bind (bounce t1) (bounce t2))
  where
    bind (Left a) (Left b) = Left (mzipWith f a b)
    bind (Left a) (Right b) = Left (mzipWith f a (return b))
    bind (Right a) (Left b) = Left (mzipWith f (return a) b)
    bind (Right a) (Right b) = Right (f a b)

interleave :: Monad m => [Trampoline m r] -> Trampoline m [r]
interleave = foldr (mzipWith (:)) (return [])