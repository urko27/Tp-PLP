module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr :: (Float -> b) ->                           -- constructor Constante
            (Float -> Float -> b) ->                  -- constructor Rango
            (b -> b -> Expr -> Expr -> b) ->          -- constructor Suma
            (b -> b -> Expr -> Expr -> b) ->          -- constructor Resta
            (b -> b -> Expr -> Expr -> b) ->          -- constructor Mult
            (b -> b -> Expr -> Expr -> b) ->          -- constructor Div
            Expr -> b
recrExpr cCte cRan cSum cRes cMul cDiv p' = case p' of
    Const c -> cCte c
    Rango a b -> cRan a b
    Suma p q -> cSum (rec p) (rec q) p q
    Resta p q -> cRes (rec p) (rec q) p q
    Mult p q -> cMul (rec p) (rec q) p q
    Div p q -> cDiv (rec p) (rec q) p q
  where
    rec = recrExpr cCte cRan cSum cRes cMul cDiv

foldExpr :: (Float -> b)          -- constructor Constante
         -> (Float -> Float -> b) -- constructor Rango
         -> (b -> b -> b)         -- constructor Suma
         -> (b -> b -> b)         -- constructor Resta
         -> (b -> b -> b)         -- constructor Mult
         -> (b -> b -> b)         -- constructor Div
         -> Expr -> b
foldExpr cCte cRan cSum cRes cMul cDiv p =
  case p of
    Const c -> cCte c
    Rango a b -> cRan a b
    Suma p q  -> cSum (rec p) (rec q)
    Resta p q -> cRes (rec p) (rec q)
    Mult p q  -> cMul (rec p) (rec q)
    Div p q   -> cDiv (rec p) (rec q)
  where
    rec = foldExpr cCte cRan cSum cRes cMul cDiv

eval :: Expr -> Gen -> (Float, Gen)
eval e = foldExpr
  (\c -> \g -> (c, g))                 -- Constructor Constante
  (\l u -> \g -> dameUno (l, u) g)     -- Constructor Rango
  (\r q -> \g -> (procesar (+) r q g)) -- Constructor Suma
  (\r q -> \g -> (procesar (-) r q g)) -- Constructor Resta
  (\r q -> \g -> (procesar (*) r q g)) -- Constructor Mult
  (\r q -> \g -> (procesar (/) r q g)) -- Constructor Div
  e
  where
    num e g = fst (e g)
    gen e g = snd (e g)
    procesar op e1 e2 g = (op (num e1 g) (num e2 (gen e1 g)), gen e2 (gen e1 g))

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst mst)) (fst mst), snd mst)
  where
    mst = muestra f n g

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.

mostrar :: Expr -> String
mostrar = recrExpr
  (\c -> show c)
  (\a b -> show a ++ "~" ++ show b)
  (\s1 s2 p q -> 
    mostrarOpDiffs p p s1 s2 [CEConst, CESuma, CERango] [CEConst, CESuma, CEResta, CEMult, CEDiv] " + ")
  (\s1 s2 p q -> 
    mostrarOp p q s1 s2 [CEConst, CERango, CEMult, CEDiv] " - ")
  (\s1 s2 p q -> 
    mostrarOp p q s1 s2 [CEConst, CERango, CEMult] " * ")
  (\s1 s2 p q -> 
    mostrarOp p q s1 s2 [CEConst, CERango] " / ")
  where
    processParen t cstrs = maybeParen(not (elem (constructor t) cstrs))
    mostrarOp p q s1 s2 cstrs op = processParen p cstrs s1 ++ op ++ processParen q cstrs s2
    mostrarOpDiffs p q s1 s2 cstrsP cstrsQ op = processParen p cstrsP s1 ++ op ++ processParen q cstrsQ s2

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
