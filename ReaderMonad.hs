import Control.Monad
import Control.Applicative

data R e a = R (e -> a)

instance Functor (R e) where
  fmap f (R x) = R $ \e -> (f . x) e

instance Applicative (R e) where
  pure x          = R $ const x
  (R f) <*> (R x) = R $ \e -> f e (x e)

instance Monad (R e) where
  return   = R . const
  x >>= f  = R $ \e -> runR (f (runR x e)) e

runR :: R e a -> e -> a
runR (R f) = f

ask :: R a a
ask = R id

asks :: (e -> a) -> R e a
asks f = do
  e <- ask
  return $ f e

local :: (e -> t) -> R t a -> R e a
local f r = do
  e <- ask
  return $ runR r (f e)

myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: R String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++ "dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
