module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
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

foldExpr :: (Float -> b) ->       
            (Float -> Float -> b) ->  -- constructor Rango
            (b -> b -> b) ->          -- constructor Suma 
            (b -> b -> b) ->          -- constructor Resta 
            (b -> b -> b) ->          -- constructor Mult 
            (b -> b -> b) ->          -- constructor Div 
            Expr -> b
foldExpr cCte cRan cSum cRes cMul cDiv p = case p of
                                    Const c -> cCte c
                                    Rango a b -> cRan a b
                                    Suma p q -> cSum (rec p) (rec q)
                                    Resta p q -> cRes (rec p) (rec q)
                                    Mult p q -> cMul (rec p) (rec q)
                                    Div p q -> cDiv (rec p) (rec q)
                                where
                                    rec = foldExpr cCte cRan cSum cRes cMul cDiv
                                    
-- recrExpr :: ... anotar el tipo ...
recrExpr :: (Float -> Expr -> (b, Gen))
         -> (Float -> Float -> Gen -> Expr -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> Expr -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> Expr -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> Expr -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> Expr -> (b, Gen))
         -> Expr -> Gen -> (b, Gen)
recrExpr cCte cRan cSum cRes cMul cDiv p' g =
  case p' of
    Const c -> cCte c p'
    Rango a b -> cRan a b g p'
    Suma p q  -> handleExpr cSum p q g p'
    Resta p q -> handleExpr cRes p q g p'
    Mult p q  -> handleExpr cMul p q g p'
    Div p q   -> handleExpr cDiv p q g p'
  where
    rec = recrExpr cCte cRan cSum cRes cMul cDiv
    handleExpr op p q gen p' = op (rec p gen) (rec q (snd (rec p gen))) p'

foldEval :: (Float -> (b, Gen))
         -> (Float -> Float -> Gen -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> (b, Gen))
         -> ( (b, Gen) -> (b, Gen) -> (b, Gen))
         -> Expr -> Gen -> (b, Gen)
foldEval cCte cRan cSum cRes cMul cDiv p g =
  case p of
    Const c -> cCte c
    Rango a b -> cRan a b g
    Suma p q  -> handleExpr cSum p q g
    Resta p q -> handleExpr cRes p q g
    Mult p q  -> handleExpr cMul p q g
    Div p q   -> handleExpr cDiv p q g
  where
    rec = foldEval cCte cRan cSum cRes cMul cDiv
    handleExpr op p q gen = op (rec p gen) (rec q (snd (rec p gen)))

eval :: Expr -> Gen -> (Float, Gen)
eval e g = foldEval
  (\c -> (c, g)) -- Constructor for Const
  (\a b gen -> dameUno (a, b) gen) -- Constructor for Rango
  (\(resP, genP) (resQ, genQ) -> (resP + resQ, genQ)) -- Constructor for Suma
  (\(resP, genP) (resQ, genQ) -> (resP - resQ, genQ)) -- Constructor for Resta
  (\(resP, genP) (resQ, genQ) -> (resP * resQ, genQ)) -- Constructor for Mult
  (\(resP, genP) (resQ, genQ) -> (resP / resQ, genQ)) -- Constructor for Div
  e g

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst mst)) (fst mst), g)
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
mostrar = foldExpr 
                  (show)
                  (\a b -> a ++ "~" ++ b) 
                  (\izq der-> izq ++ "+" ++ der)
                  (\izq der-> izq ++ "-" ++ der)
                  (\izq der-> izq ++ "*" ++ der)
                  (\izq der-> izq ++ "/" ++ der)

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
