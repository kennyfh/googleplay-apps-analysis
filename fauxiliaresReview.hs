import Data.Ord (comparing)
import Data.Array
import Data.Char
import Data.List
import Control.Exception (catch, SomeException)
import PilaConTipoDeDatoAlgebraic


-- Define la función (limpia cs), tal que reciba una cadena de caracteres 
-- y devuelva otra donde solo queden letras del abecedario en inglés
-- (sin ñ, ç, tildes, ...) convertidas a minúsculas.


--  limpia "I like eat delicious food. That's I'm cooking food myself, case \"10 Best Foods\" helps lot, also \"Best Before (Shelf Life)\""
-- "ilikeeatdeliciousfoodthatsimcookingfoodmyselfcasebestfoodshelpslotalsobestbeforeshelflife"


limpia :: String -> String
limpia str = filter (\x -> elem x abcario) $ map (\x-> toLower x) str -- 1º convertimos todo a minúsculas 
  -- luego vemos si son letras del abecedario ingles
  where abcario = ['a'..'z'] -- lista de elementos del abecedario ingles

-- ----------------------------------------------------------------------


-- Define el tipo sinónimo TablaFrecuencias tal que sea un vector (es decir, 
-- un Array de 1 dimensión) cuyos índices sean enteros y los valores sean
-- números reales.

--TablaFrecuencias = ....
-- type Vector a = Array Int a
type TablaFrecuencias = Array Int Float

-- ----------------------------------------------------------------------

-- Define la función (frecuencias cs) tal que reciba un String y devuelva 
-- un valor del tipo TablaFrecuencias. La cadena cs debe ser procesada
-- por la función 'limpia' del ejercicio 1. La tabla de frecuencias debe 
-- tener una posición para cada letra del abecedario en inglés (26 en toal),
-- siendo el primer elemento la frecuencia de la 'a', el segundo el de la 'b'
-- ... el último el de la 'z'. Si una letra no aparece en cs, su frecuencia
-- es 0. 
-- NOTA: si no has hecho el primer ejercicio, asume que cs contiene solo letras
-- en minúsculas.
-- Por ejemplo,
-- > frecuencias "ABRRKBAARAA"
-- array (0,25) [(0,0.45454547),(1,0.18181819),(2,0.0),(3,0.0),(4,0.0),(5,0.0),
-- (6,0.0),(7,0.0),(8,0.0),(9,0.0),(10,9.090909e-2),(11,0.0),(12,0.0),(13,0.0),
-- (14,0.0),(15,0.0),(16,0.0),(17,0.27272728),(18,0.0),(19,0.0),(20,0.0),(21,0.0),
-- (22,0.0),(23,0.0),(24,0.0),(25,0.0)]
-- > head (elems (frecuencias "AaAa Aa"))
-- 1.0

frecuencias :: String -> TablaFrecuencias
frecuencias cs = listArray (0,25) asd -- creamos el vector tipo TablaFrecuencias
  where asd = (modificaElem $ frecuenciasCadena $ limpia cs) -- Aqui calculamos la frecuencia de cada letra del abecedario
  --   where asd = zip [0..25] (modificaElem $ frecuenciasCadena $ limpia cs)


modificaElem :: [Float] -> [Float] -- lo devolvemos como si fuera frecuencia, porque estaba en porcentaje
modificaElem (x:xs)
  | x/=0.0 = [x / 100] ++ modificaElem xs
  | otherwise = [0.0] ++ modificaElem xs

frecuenciasCadena :: String -> [Float] -- Frecuencia de cada una de las letras de la cadena xs
frecuenciasCadena xs = 
    [porcentaje (ocurrencias x xs') n | x <- ['a'..'z']] 
    where xs' = [toLower x | x <- xs]
          n   = length (xs)

ocurrencias :: Eq a => a -> [a] -> Int -- Es el numero de veces que se repite el elemento x en la cadena xs
ocurrencias x xs = length [x' | x' <- xs, x == x'] -- Por cada elemento de la cadena, si es igual al pasado
                                                    -- como parámetro, guardarlo en la lista y posteriormente, 
                                                    -- cuando hayas recorrido toda la cadena, devolver la longitud de esa nueva lista

porcentaje :: Int -> Int -> Float -- es el porcentaje de n sobre m
porcentaje n m = (fromIntegral n / fromIntegral m) * 100


-- Define la función (pilaFrecuencias tf) tal que reciba una tabla de 
-- frecuencias y devuelva una pila cuyos elementos sean pares (c,f), 
-- donde c es un caracter y f es su frecuencia. Los elementos en la 
-- pila deben estar ordenados de mayor a menor por frecuencia (es decir,
-- la cima de la pila es el caracter con mayor frecuencia), y solo 
-- deben aparecer aquellos cuya frecuencia no sea 0.
-- NOTA: Si no has podido hacer el ejercicio 3, puedes usar la variable
-- 'tf' del ejemplo siguiente.
-- Por ejemplo,
-- > let tf = array (0,3) [(0,0.16666667),(1,0.33333334),(2,0.5),(3,0.0)]
-- > pilaFrecuencias tf
-- ('c',0.5)|('b',0.33333334)|('a',0.16666667)|-
-- > pilaFrecuencias $ frecuencias "ABRRKBAARAA"
-- ('a',0.45454547)|('r',0.27272728)|('b',0.18181819)|('k',9.090909e-2)|-

pilaFrecuencias :: TablaFrecuencias -> Pila (Char, Float)
pilaFrecuencias tabla = foldr apila vacia (ordenarElementos $ filtradoTabla tabla) 
-- Lo que buscamos es ordenar de mayor a menor las frecuencias que sean distintas de cero
-- para que a la hora de insertarlas en la pila vayan en el orden acordado

-- Funcion que haga que nos devuelva la lista de pares ordenados
ordenarElementos :: [(Char,Float)] -> [(Char,Float)]
ordenarElementos tuplas = reverse $ sortBy (comparing (\(x,y) -> y)) tuplas -- comparamos por las frecuencias y luego le 
                                                                            -- le damos la vuelta para que vayan de mayor a menor

-- (filtradoTabla tabla) Dada una tabla de Frecuencias, nos devolverá una lista de pares donde se encuentran los
-- caracteres con su frecuencia que sean distintas de cero
-- filtradoTabla :: TablaFrecuencias -> [(Char,Float)]
filtradoTabla tabla = filter (\(a,b) -> b/=0.0) $ zip ['a'..'z'] ls
  where ls = elems tabla
-- ----------------------------------------------------------------------


