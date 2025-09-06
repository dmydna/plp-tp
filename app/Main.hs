module Main where

import App
import Generador
import TinyApp.Repl

main :: IO ()
main = do
  g <- genNormal
  runRepl (app g)
-- Borrador (no testeado)

-- module Util

alinearDerecha :: Int -> String -> String
alinearDerecha n s = replicate (n - (length s)) " " ++ s

actualizarElem :: Int -> (Int -> Int ) -> [Int] -> [Int]
actualizarElem n f xs = zipWith (\x y -> if x==n then f x else y) [0 .. ((length xs)-1)] xs

-- module Histograma

{--Histograma i t cs: 
       para cada casillero (x,y) entonces x-y = t
--}


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


--casilleros :: Histograma -> [Casillero]
--casillero (Histograma i t cs) = zipWith (\idx n -> Casillero min max cantidad porcentaje) 
--                                [0 .. (length cs)] 
--                                cs
--                               where min = if idx==0 then infinitoNegativo else (i + (idx-2) * t)
--                                      max = if idx==(length cs)-1 then infinitoPositivo else (i + (idx-1) * t)
--                                      cantidad = n
--                                      porcentaje = (n/(sum cs) * 100)




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
                                        








