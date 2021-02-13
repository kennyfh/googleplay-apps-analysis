-- Review
-- Kenny Jesús Flores Huamán
-- Jesús Pineda Marquez
-- Universidad de Sevilla
-- Sevilla, 1 febrero de 2021
-- =====================================================================


-- - METODOS QUE PODRIAMOS IMPLEMENTAR
-- 1) Numero de atributos que tiene nuestro dataset (hecho)
-- 2) Cantidad de reviews según una evaluación (hecho)
-- 3) Devolver todas las reviews de una aplicación (hecho)
-- 4) Devolver el porcentaje de caracteres usados en un comentario 

module Review
    (Review,
    Reviews,
    imprimeCabecera,
    traduccionRecords,
    traduccionRecord,
    traduce,
    traducesent,
    porcVal,
    obtenerReviews,
    imprimirRTraduccion,
    seleccionarComentario,
    pilaPorcentajes
    ) where

-- =====================================================================
-- MODULOS UTILIZADAS
import Text.CSV -- Implementación de csv en Haskell
import Data.Default -- Librería que nos permite instanciar la clase Default
import Data.List as L
import Data.Ord (comparing)
import Data.Maybe
import Data.Array
import Data.Char
import PilaConTipoDeDatoAlgebraico

-- =====================================================================


-- =====================================================================
-- TIPOS DE DATOS UTILIZADOS

-- Creación de un nuevo tipo de datos, que corresponderá con cada fila de atributos
-- que contiene nuestro dataset
data Review = Rev {
    app::String, translated_Review::String, sentiment::String, sentiment_Polarity::Float, sentiment_Subjectivity::Float

} deriving (Show, Eq)

-- Creación de una instancia para tener valores por defecto en el caso de que falten campos 
-- en nuestra Review
instance Default Review where
    def =  Rev {
    app=def, translated_Review=def ,sentiment=def ,sentiment_Polarity=def ,sentiment_Subjectivity=def
    }

type Reviews = [Review] -- Lista de Reviews
-- =====================================================================


-- FUNCIONES
-- =====================================================================
-- (imprimeCabecera xs) procesa la cabecera, imprimiendo por cada campo de la misma su número y la línea.
-- Por ejemplo:
-- *Main> let ej = ["app","translated_Review","sentiment","sentiment_Polarity"]::Record
-- *Main> imprimeCabecera ej
-- 1 : app
-- 2 : translated_Review
-- 3 : sentiment
-- 4 : sentiment_Polarity   
imprimeCabecera :: Record -> IO()
imprimeCabecera xs = sequence_ $ map (\ (x,y) -> putStrLn $ concat $ [show x," : ",y]) (zip [1..] xs)

-- =====================================================================
-- =====================================================================
-- (traduccionRecords cabecera xs) Dada una lista de Strings con la cabecera del csv "cabecera" y 
-- una lista de Records "xs", obtiene un tipo Reviews

traduccionRecords :: [String] -> [Record] -> Reviews
traduccionRecords cabecera xs = [traduccionRecord cabecera x | x<-xs]
-- =====================================================================
-- =====================================================================

-- (traduccionRecord cabecera xs) dada una lista de Strings con la cabecera del csv y un Record,
-- obtiene un tipo Review. Por ejemplo:

traduccionRecord :: [String] -> Record -> Review
traduccionRecord cabecera xs = variable
    where ts =  [(a,b) | (a,b) <- (zip cabecera xs)]
          variable = def {
          app=(traduce ts "app"), translated_Review=(traduce ts "translated_Review"), sentiment=(traduce ts "sentiment"), 
          sentiment_Polarity=(traducesent (traduce ts "sentiment_Polarity")), sentiment_Subjectivity=(traducesent (traduce ts "sentiment_Subjectivity"))
          }

-- =====================================================================
-- (traduce xs str) dada una listas de tuplas "xs" y un string "str",
-- va a devolver el segundo elemento de la tupla si el primer elemento de la tupla es igual al "str".

traduce :: [(String,String)] -> String -> String
traduce xs str = head [b | (a,b)<-xs, a==str]
-- =====================================================================
-- =====================================================================

-- (traducesent str) es la traducción de la polaridad o subjetividad que tiene un sentimiento. Por ejemplo:
--    traducesent "0.4" == 0.4

traducesent :: String -> Float
traducesent str = read str :: Float
-- =====================================================================
-- =====================================================================

-- (numVal rws sent) Nos devuelve el número de comentarios que tienen un tipo de sentimiento (sent).
numVal :: Reviews -> String -> Int
numVal (x:xs) a
        | null xs = res
        | otherwise = res + numVal xs a
        where res = if sentiment x == a then  1 else 0

-- =====================================================================
-- =====================================================================
-- (porVal xs sent) Nos muestra por pantalla el porcentaje de comentarios del dataset que tienen el mismo tipo de sentimiento
-- en el que estamos pasando como parámetro (sent).
porcVal :: Reviews ->  String -> IO()
porcVal _ "" = error "Debe añadir un elemento tras la lista de Reviews dadas"
porcVal [] _ = error "La lista de Reviews no puede estar vacía"
porcVal xs "Positive" =  putStrLn $ "El " ++ show (fromIntegral $ numVal xs "Positive" * 100 `div` length xs ) ++ "% son reviews positivas"
porcVal xs "Neutral" = putStrLn $ "El " ++ show (fromIntegral $ numVal xs "Neutral" * 100 `div` length xs ) ++ "% son reviews Neutrales"
porcVal xs "Negative" = putStrLn $ "El " ++ show (fromIntegral $ numVal xs "Negative" * 100 `div` length xs ) ++ "% son reviews Negativas"
porcVal xs _ = error "Las reviews solamente pueden ser Positive, Negative o Neutral"
-- =====================================================================
-- =====================================================================

-- (obtenerReviews str rws) Dado el nombre de una aplicación (str) y una lista de Reviews, nos devuelve todas las aplicaciones
-- que son el mismo nombre que el pasado como parámetro (str). Por ejemplo:

-- let x = let x = [Rev {app = "PD", translated_Review = "GreatApp", sentiment = "Negative", sentiment_Polarity = -2.5e-2, sentiment_Subjectivity = 0.125}, Rev {app = "ISLU", translated_Review = "Pathetic app.", sentiment = "Negative", sentiment_Polarity = -0.3625, sentiment_Subjectivity = 0.625}]::Reviews

-- Review> obtenerReviews "PD" x

-- Review> [Rev {app = "PD", translated_Review = "GreatApp", sentiment = "Negative", sentiment_Polarity = -2.5e-2, sentiment_Subjectivity = 0.125}]

obtenerReviews :: String -> Reviews -> [String]
obtenerReviews str (r:rws)
    | null rws = f (app r) r
    | otherwise = (f (app r) r) ++ (obtenerReviews str rws)
    where f x r
           | x==str = [translated_Review r]
           | otherwise = []
-- =====================================================================
-- =====================================================================
-- (imprimirRTraduccion xs) Dada una lista de comentarios de cierta aplicación, nos mostrará por pantalla
-- todos los comentarios traducidos al inglés que tenga esa app
imprimirRTraduccion :: [String] -> IO()
imprimirRTraduccion xs = mapM_ (\ (a,b) -> putStrLn $ concat $ [show a, " : ", b]) comments
    where comments = zip [1..] xs

-- =====================================================================
-- =====================================================================
-- Selecciona el comentario según la posición que le indicas
seleccionarComentario :: Int -> [String] -> String
seleccionarComentario pos xs
    | pos > (length xs) =  error "No se puede seleccionar un comentario con un número inválido"
    | otherwise = xs !! (pos-1)

-- =====================================================================
-- =====================================================================
-- (limpia c) Recibe un comentario y nos devuelve el mismo comentario pero
-- eliminando todas los caracteres que no sean letras del alfabeto inglés,
-- convertidas a minúsculas. Por ejemplo:
--  limpia "I like eat delicious food. That's I'm cooking food myself, case \"10 Best Foods\" helps lot, also \"Best Before (Shelf Life)\""
-- "ilikeeatdeliciousfoodthatsimcookingfoodmyselfcasebestfoodshelpslotalsobestbeforeshelflife"


limpia :: String -> String
limpia str = filter (\x -> elem x abcario) $ map (\x-> toLower x) str 
  where abcario = ['a'..'z']

-- =====================================================================
-- =====================================================================
pilaPorcentajes :: String -> Pila (Char, Float)
pilaPorcentajes xs = foldr apila vacia asd
    where asd = zip ['a'..'z'] (frecuenciasCadena xs)

-- =====================================================================
-- =====================================================================
frecuenciasCadena :: String -> [Float]
frecuenciasCadena xs = [porcentaje (ocurrencias x xs') n | x <- ['a'..'z']] 
    where xs' = limpia xs
          n = length (xs)

-- =====================================================================
-- =====================================================================
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']
                                                  

porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100
