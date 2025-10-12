-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util
import Data.List (zipWith4) 

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio tamaño (inferior, superior) = Histograma inferior ((superior - inferior)/fromIntegral tamaño) (replicate (tamaño+2) 0)

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma l tamIntervalo cs) = Histograma l tamIntervalo (actualizarElem i (+1) cs)
  where
    i
      | x < l = 0
      | x >= l + tamIntervalo * fromIntegral (length cs - 2) = length cs - 1
      | otherwise = floor ((x - l) / tamIntervalo) + 1

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma tamaño (inf, sup) = foldr agregar (vacio tamaño (inf, sup))

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p



-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros hist = zipWith4 Casillero (limitesInferiores hist) (limitesSuperiores hist) (cantidades hist) (porcentajes hist)
  where
    limitesIntermedios :: Float -> Float -> Int -> [Float]
    limitesIntermedios primerLimite longitudIntervalo tamaño = [ fromIntegral indice * longitudIntervalo + primerLimite | indice <- [0..tamaño] ]

    limitesInferiores :: Histograma -> [Float]
    limitesInferiores (Histograma primerLimite longitudIntervalo _cantidades) = infinitoNegativo : limitesIntermedios primerLimite longitudIntervalo (length _cantidades -2)

    limitesSuperiores :: Histograma -> [Float]
    limitesSuperiores (Histograma primerLimite longitudIntervalo _cantidades) = limitesIntermedios primerLimite longitudIntervalo (length _cantidades -2) ++ [infinitoPositivo]

cantidades :: Histograma -> [Int]
cantidades (Histograma _ _ _cantidades) = _cantidades

porcentajes :: Histograma -> [Float]
porcentajes (Histograma _ _ _cantidades) = if sumaCantidades == 0
                                            then [ 0 | _ <- [0..length _cantidades -1] ]
                                            else map (\cantidad -> cantidad * 100.0 / fromIntegral(sumaCantidades)) (map fromIntegral _cantidades)
                                            where sumaCantidades = sum _cantidades
                                            