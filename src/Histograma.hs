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

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.

vacio :: Int -> (Float, Float) -> Histograma
vacio n (i, t) = Histograma i t (replicate n 0)

posicion :: Float -> Float -> Float -> Int -> Int
posicion n i t len | n < i = 0
                   | otherwise = min (floor ((n-i)/t)) len


agregar :: Float -> Histograma -> Histograma
agregar n (Histograma i t cs) = 
                Histograma i t 
                (actualizarElem (posicion n i t (length cs)) ( +1 ) cs)
        

histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n rango =  foldr(\x rec -> agregar x rec) (vacio n rango) 


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
minimos :: Int -> Float -> Float -> [Float]
minimos n i t =  infinitoNegativo:[ i+x*t | x<- [0..(n-1)] ]

maximos :: Int -> Float -> Float -> [Float]
maximos n i t =  [ i+x*t | x<- [0..(n-1)] ]++[infinitoPositivo]

porcentajes :: Int -> [Int] -> [Float]
porcentajes total = foldr (\cant r -> ( (cant * 100)/total):rec) [] 
                              
casilleros :: Histograma -> [Casillero]
casilleros (Histograma i t cs) = zipWith4 (
                                          \min max cantidad porcentaje -> Casillero min max cantidad porcentaje
                                        ) (minimos (length cs) i t )
                                         (maximos (length cs) i t )
                                          cs 
                                          (porcentajes (sum cs) cs)
                                        


