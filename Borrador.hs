-- Borrador (no testeado)

-- module Util

alinearDerecha :: Int → String → String
alinearDerecha n s = replicate (n - (length s)) " " ++ s

actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f = zipWith (\x y -> if x==n then f x else y) [0 .. ((length xs)-1)]

-- module Histograma

{--Histograma i t cs: 
       para cada casillero (x,y) entonces x-y = t
--}

data Histograma = Histograma Float Float [ Int ]

vacio :: Int -> (Float, Float) -> Histograma
vacio n (i, t) = Histograma i t (replicate n 0)


agregar :: Float → Histograma → Histograma
agregar n = (\i t cs ->  Histograma i t (actualizarElem pos (+1) cs)
           where pos  | n < i = 0
                      | otherwise = min (floor (n-i)/t) (length cs)
        

histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n rango =  foldr(\x rec -> agregar x rec) (vacio n rango) 


casilleros :: Histograma -> [Casillero]
casillero (Histograma i t cs) = zipWith (\idx n -> Casillero min max cantidad porcentaje) 
                                [0 .. (length cs)] 
                                cs
                                where min = if idx==0 then infinitoNegativo else (i + (idx-2) * t)
                                      max = if idx==(length cs)-1 then infinitoPositivo else (i + (idx-1) * t)
                                      cantidad = n
                                      porcentaje = (n/(sum cs) * 100)








