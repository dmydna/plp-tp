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


-- recrExpr :: ... anotar el tipo ...
recExpr ::  (Expr -> Expr -> G b -> G b ->  G b) ->  -- Div
            (Expr -> Expr -> G b -> G b ->  G b) ->  -- Suma
            (Expr -> Expr -> G b -> G b ->  G b) ->  -- Resta
            (Expr -> Expr -> G b -> G b ->  G b) ->  -- Mult
            (Float -> Float -> G b) ->               -- Rango
            (Float ->G b) ->                         -- Const
            Expr ->                                   
            G b
recExpr fdiv fmul fres fsum frng fcst e gen =  case e of  
                    Const x     -> fcst x gen   -- tipo: Gen -> (b, Gen)
                    Rango x y   -> frng x y gen -- tipo : Gen -> (b, Gen)
                    Suma  e1 e2 -> fsum e1 e2 (rec e1) (rec e2) gen  
                    Resta e1 e2 -> fres e1 e2 (rec e1) (rec e2) gen
                    Mult  e1 e2 -> fmul e1 e2 (rec e1) (rec e2) gen
                    Div   e1 e2 -> fdiv e1 e2 (rec e1) (rec e2) gen
                    where rec = recExpr fdiv fmul fres fsum frng fcst


foldExpr :: (G b -> G b ->  G b) ->    -- Div
            (G b -> G b ->  G b) ->    -- Mult
            (G b -> G b ->  G b) ->    -- Rest
            (G b -> G b ->  G b) ->    -- Fsum
            (Float -> Float -> G b) -> -- Rango
            (Float -> G b)  ->         -- Const
            Expr -> 
            G b
foldExpr fdiv fmul fres fsum frng fcst e gen  =  case e of  
                       Const x   ->  fcst x gen    -- tipo: Gen -> (b, Gen)
                       Rango x y ->  frng x y gen  -- tipo : Gen -> (b, Gen)
                       Suma  e1 e2 -> fsum (rec e1) (rec e2) gen 
                       Resta e1 e2 -> fres (rec e1) (rec e2) gen
                       Mult  e1 e2 -> fmul (rec e1) (rec e2) gen
                       Div   e1 e2 -> fdiv (rec e1) (rec e2) gen
                       where rec = foldExpr fdiv fmul fres fsum frng fcst




-- foldExpr1 :: (b-> b ->  G b) ->
--              ( b -> b ->  G b) -> 
--              ( b ->  b ->  G b) ->
--              ( b ->  b ->  G b) -> 
--              ( Float -> Float -> G b) -> 
--              (Float ->G b) -> 
--              Expr -> 
--              G b
-- foldExpr1 fdiv fmul fres fsum frng fcst (Const x) = fcst x  -- tipo: Gen -> (b, Gen)
-- foldExpr1 fdiv fmul fres fsum frng fcst (Rango x y) = frng x y -- tipo : Gen -> (b, Gen)
-- foldExpr1 fdiv fmul fres fsum frng fcst (Suma s1 s2) gen = fsum res1 res2 g2
--                                                           where rcall = foldExpr fdiv fmul fres fsum frng fcst 
--                                                                (res1, g1) = rcall s1 gen
--                                                                (res2, g2) = rcall s2 g1
--                     foldExpr1 fdiv fmul fres fsum frng fcst (Resta r1 r2) = fres res1 res2 g2
--                                                           where rcall = foldExpr fdiv fmul fres fsum frng fcst 
--                                                                (res1, g1) = rcall s1 gen
--                                                                (res2, g2) = rcall s2 g1
--                      foldExpr1 fdiv fmul fres fsum frng fcst (Mult m1 m2) = fmul res1 res2 g2
--                                                           where rcall = foldExpr fdiv fmul fres fsum frng fcst 
--                                                                (res1, g1) = rcall s1 gen
--                                                                (res2, g2) = rcall s2 g1
--                       foldExpr1 fdiv fmul fres fsum frng fcst (Div d1 d2) = fdiv res1 res2 g2
--                                                             where rcall = foldExpr fdiv fmul fres fsum frng fcst 
--                                                                 (res1, g1) = rcall s1 gen
--                                                                 (res2, g2) = rcall s2 g1


-- List a = [] | a:(List a)

-- x:x:x:x:[]

-- | Evaluar expresiones dado un generador de números aleatorios
--eval :: Expr -> G Float
--eval expr gen =  foldExpr 
--                  (\f1 f2 -> \gen -> (v1 + v2, g2) 
--                  where (v1, g1) = f1 gen
--                        (v2, g2) = f2 g1)       -- fdiv
--                  (\f1 f2 -> \gen -> (v1 * v2, g2) 
--                  where (v1, g1) = f1 gen
--                        (v2, g2) = f2 g1)       -- fmul
--                  (\f1 f2 -> \gen -> (v1 - v2, g2) 
--                  where (v1, g1) = f1 gen
--                        (v2, g2) = f2 g1)       -- fres
--                  (\f1 f2 -> \gen -> (v1 + v2, g2)
--                  where (v1, g1) = f1 gen
--                        (v2, g2) = f2 g1 )       -- fsum
--                  (\x  y  -> \gen -> dameUno (x , y) gen ) -- frng
--                  (\x     -> \gen -> (x, gen) )            -- fcst



-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval expr gen =  foldExpr 
        (\f1 f2 -> evalExpr (f1 gen) (f2 (snd(f1 gen))) (/) )    -- fdiv       
        (\f1 f2 -> evalExpr (f1 gen) (f2 (snd(f1 gen))) (*) )    -- fmul
        (\f1 f2 -> evalExpr (f1 gen) (f2 (snd(f1 gen))) (-) )    -- frest
        (\f1 f2 -> evalExpr (f1 gen) (f2 (snd(f1 gen))) (+) )    -- fsum
        (\x  y  -> \gen -> dameUno (x , y) gen ) -- frng
        (\x     -> \gen -> (x, gen) )            -- fcst
        expr
        gen


evalExpr :: (Float, Gen) -> (Float, Gen) -> (Float -> Float -> Float) -> G Float
evalExpr    (v1, g1) (v2, g2) op  = \gen -> (v1 `op` v2, g2)


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
--armarHistograma :: Int -> Int -> ( Gen -> (Float, Gen) )-> Gen -> (Histograma, Gen)
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f  = (\g  -> let 
                  (vs, g1) = (muestra f n g) 
                  (inicio,fin) = rango95 vs  
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
mostrar e =  fst (foldExpr 
      (\f1 f2 -> \gen -> let 
                  (v1, g1) = f1 gen 
                  (v2, g2) = f2 g1  
                 in ( v1  ++ ['/'] ++ v2, g2)
      )
        
      (\f1 f2 -> \gen -> let 
                  (v1, g1) = f1 gen 
                  (v2, g2) = f2 g1  
                 in ( v1  ++ ['*'] ++ v2, g2)
      )       -- fdiv           -- fmul
      (\f1 f2 -> \gen -> let 
                  (v1, g1) = f1 gen 
                  (v2, g2) = f2 g1  
                 in ( v1  ++ ['-'] ++ v2, g2)
      )      -- fdiv       -- fres
      (\f1 f2 -> \gen -> let 
                  (v1, g1) = f1 gen 
                  (v2, g2) = f2 g1  
                 in ( v1  ++ ['+'] ++ v2, g2)
      )      -- fdiv     -- fsum
      (\f1 f2 -> \gen -> let 
                  (v1, g1) = f1 gen 
                  (v2, g2) = f2 g1  
                 in ( v1  ++ ['∼'] ++ v2, g2)
      )       -- fdiv
      (\x     -> \gen ->  (show x, gen) )  
      e  
      genFijo )       -- fcst



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
