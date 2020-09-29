-- Jake Lovingood Homework 5
-- 9/22/20
-- CS 3490
-- Type Declaration of a Poly.
import Control.Applicative hiding (Const)

data Poly a = Var a | Const Double | Add (Poly a) (Poly a) | Mul (Poly a) (Poly a)
     deriving (Show,Read,Eq)

-- Is it a functor? It is if we can define this function.
mapPoly :: (a -> b) -> Poly a -> Poly b
mapPoly f (Var x) = Var (f x)
mapPoly f (Const x) = Const x
mapPoly f (Add p1 p2) = Add (mapPoly f p1) (mapPoly f p2)
mapPoly f (Mul p1 p2) = Mul (mapPoly f p1) (mapPoly f p2)

-- We can now declare that Poly is indeed a functor
instance Functor Poly where
   fmap f p = mapPoly f p

-- Is poly a Monad?
-- It is, if we can define the following functions
unitPoly :: a -> Poly a
unitPoly x = Var x

-- BindPoly
bindPoly :: (a -> Poly b) -> Poly a -> Poly b
bindPoly f (Var x) = f x
bindPoly f (Const x) = Const x
bindPoly f (Add p1 p2) = Add (bindPoly f p1) (bindPoly f p2)
bindPoly f (Mul p1 p2) = Mul (bindPoly f p1) (bindPoly f p2)


-- ApplyPoly
applyPoly :: Poly (a -> b) -> Poly a -> Poly b
applyPoly pf pa = bindPoly (\f -> fmap f pa) pf

--Instance declarations
instance Applicative Poly where 
  p <*> q = applyPoly p q 
  pure x = unitPoly x
--Instance declaration
instance Monad Poly where
  return x = unitPoly x
  (>>=) p f = bindPoly f p
-- End of Instance
