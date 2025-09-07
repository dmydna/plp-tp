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
recExpr :: (Expr -> b -> b -> b ) -> (Expr -> b -> b -> b) -> (Expr -> b -> b -> b) -> (Expr -> b -> b -> b) -> (Expr -> Float -> Float -> b) -> (Expr -> Float -> b) -> Expr -> b
recrExpr fdiv fmul fres fsum frng fcst expr =  case expr of  
                                                   Const x -> fcst (Const x) x 
                                                   Rango x y -> frng (Rango x y) x y
                                                   Suma x y -> fsum (Suma x y) (rec x) (rec y)
                                                   Resta x y -> fres (Resta x y) (rec x) (rec y)
                                                   Mult x y -> fmul (Mult x y ) (rec x) (rec y)
                                                   Div x y -> fdiv (Div x y) (rec x) (rec y)
                                                  where rec = foldExpr fdiv fmul fres fsum frng fcst 


foldExpr :: (b -> b -> b) -> ((b -> b -> b)) -> (b -> b -> b) -> (b -> b -> b) -> (Float -> Float -> b) -> (Float -> b) -> Expr -> b
foldExpr fdiv fmul fres fsum frng fcst expr =  case expr of  
                                                   Const x -> fcst x 
                                                   Rango x y -> frng x y
                                                   Suma x y -> fsum (rec x) (rec y)
                                                   Resta x y -> fres (rec x) (rec y)
                                                   Mult x y -> fmul (rec x) (rec y)
                                                   Div x y -> fdiv (rec x) (rec y)
                                                  where rec = foldExpr fdiv fmul fres fsum frng fcst

-- List a = [] | a:(List a)

-- x:x:x:x:[]

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval expr gen =  (foldExpr 
                  ( \x y -> x/y ) -- fdiv
                  ( \x y -> x*y ) -- fmul
                  ( \x y -> x-y ) -- fres
                  ( \x y -> x+y ) -- fsum
                  ( \x y -> dameUno (x , y) gen ) -- frng
                  ( \x -> x ) -- fcst
                  , gen) -- Retornamos una tupla con un resultado y un generador
                


-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
--armarHistograma :: Int -> Int -> ( Gen -> (Float, Gen) )-> Gen -> (Histograma, Gen)
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

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
