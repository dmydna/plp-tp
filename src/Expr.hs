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



recrExpr ::   (  Expr -> b-> b ->  G b) -> -- fdiv (Div e1 e2) b1 b2 
             (  Expr ->b -> b ->  G b) -> 
             (  Expr -> b ->  b ->  G b) ->
             (  Expr -> b ->  b ->  G b) -> 
             (  Expr -> Float -> Float -> G b) -> 
             (  Expr ->Float ->G b) -> 
             Expr -> 
             G b
recrExpr fdiv fmul fres fsum frng fcst (Const x) gen = fcst (Const x) x gen  -- tipo: Gen -> (b, Gen)
recrExpr fdiv fmul fres fsum frng fcst (Rango x y) gen = frng (Rango x y) x y gen -- tipo : Gen -> (b, Gen)
recrExpr fdiv fmul fres fsum frng fcst (Suma s1 s2) gen = fsum (Suma s1 s2) res1 res2 g2
                                                            where rcall = recrExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall s1 gen
                                                                  (res2, g2) = rcall s2 g1
recrExpr fdiv fmul fres fsum frng fcst (Resta r1 r2) gen = fres (Resta r1 r2) res1 res2 g2
                                                            where rcall = recrExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall r1 gen
                                                                  (res2, g2) = rcall r2 g1
recrExpr fdiv fmul fres fsum frng fcst (Mult m1 m2) gen = fmul (Mult m1 m2) res1 res2 g2
                                                            where rcall = recrExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall m1 gen
                                                                  (res2, g2) = rcall m2 g1
recrExpr fdiv fmul fres fsum frng fcst (Div d1 d2) gen = fdiv (Div d1 d2) res1 res2 g2
                                                            where rcall = recrExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall d1 gen
                                                                  (res2, g2) = rcall d2 g1




foldExpr :: ( b-> b ->  G b) ->
             ( b -> b ->  G b) -> 
             ( b ->  b ->  G b) ->
             ( b ->  b ->  G b) -> 
             ( Float -> Float -> G b) -> 
             (Float ->G b) -> 
             Expr -> 
             G b
foldExpr fdiv fmul fres fsum frng fcst (Const x) gen = fcst x gen  -- tipo: Gen -> (b, Gen)
foldExpr fdiv fmul fres fsum frng fcst (Rango x y) gen = frng x y gen -- tipo : Gen -> (b, Gen)
foldExpr fdiv fmul fres fsum frng fcst (Suma s1 s2) gen = fsum res1 res2 g2
                                                            where rcall = foldExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall s1 gen
                                                                  (res2, g2) = rcall s2 g1
foldExpr fdiv fmul fres fsum frng fcst (Resta r1 r2) gen = fres res1 res2 g2
                                                            where rcall = foldExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall r1 gen
                                                                  (res2, g2) = rcall r2 g1
foldExpr fdiv fmul fres fsum frng fcst (Mult m1 m2) gen = fmul res1 res2 g2
                                                            where rcall = foldExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall m1 gen
                                                                  (res2, g2) = rcall m2 g1
foldExpr fdiv fmul fres fsum frng fcst (Div d1 d2) gen = fdiv res1 res2 g2
                                                            where rcall = foldExpr fdiv fmul fres fsum frng fcst 
                                                                  (res1, g1) = rcall d1 gen
                                                                  (res2, g2) = rcall d2 g1



-- | Evaluar expresiones dado un generador de números aleatorios
eval::Expr -> G Float 
eval = foldExpr (\x y gen -> (x/y, gen))
                           (\x y gen -> (x*y, gen)) 
                           (\x y gen -> (x-y, gen))
                           (\x y gen -> (x+y, gen)) 
                           (\x y gen -> dameUno (x,y) gen) 
                           (\x gen -> (x,gen)) 

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
--armarHistograma :: Int -> Int -> ( Gen -> (Float, Gen) )-> Gen -> (Histograma, Gen)
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f  = (\g  -> let 
                                    (vs, g1) = (muestra f n g) 
                                    (inicio,fin) = rango95 vs 
                                    tamaño  =  (fin - inicio) / (fromIntegral m)
                                  in ( histograma m (inicio, tamaño) vs, g1 )
             )

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
mostrar expr = fst $ foldExpr 
                    (\x y gen -> (x++"/"++y, gen))
                    (\x y gen -> (x++"*"++y, gen))
                    (\x y gen -> (x++"-"++y, gen))
                    (\x y gen -> (x++"+"++y, gen))
                    (\x y gen -> (show x++"~"++show y, gen))
                    (\x gen -> (show x, gen)) expr genFijo



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
