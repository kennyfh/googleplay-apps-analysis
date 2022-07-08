-- Review
-- Kenny Jesús Flores Huamán
-- Jesús Pineda Marquez
-- Universidad de Sevilla
-- Sevilla, 7 febrero de 2021
-- =====================================================================

-- 
-- 1) Numero de atributos que tiene nuestro dataset
-- 2) Cantidad de reviews según una evaluación
-- 3) Devolver todas las reviews de una aplicación
-- 4) Devolver el porcentaje de caracteres usados en un comentario 
-- 5) Media del sentimiento subjetivo

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
    lPorcentajes,
    sentPolAverage,
    polAverage,
    arbolP,
    imprimeLetras
    ) where

-- =====================================================================
-- MODULOS UTILIZADAS
import Text.CSV -- Implementación de csv en Haskell
import Data.Default -- Nos permite instanciar la clase Default
import Data.List as L
import Data.Ord (comparing)
import Data.Maybe
import Data.Array
import Data.Char

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

-- (obtenerReviews str rws) Dado el nombre de una aplicación (str) y una lista de Reviews, nos devuelve los comentarios
-- que son el mismo nombre que el pasado como parámetro (str). Por ejemplo:

-- let x = let x = [Rev {app = "PD", translated_Review = "GreatApp", sentiment = "Negative", sentiment_Polarity = -2.5e-2, sentiment_Subjectivity = 0.125}, Rev {app = "ISLU", translated_Review = "Pathetic app.", sentiment = "Negative", sentiment_Polarity = -0.3625, sentiment_Subjectivity = 0.625}]::Reviews

-- Review> obtenerReviews "PD" x

-- Review> ["GreatApp"]

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
-- (seleccionarComentario pos xs) Selecciona el comentario según la posición que le indicas
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
-- (lPorcentajes xs) Dada una review traducida al inglés, devuelve una
-- lista de pares donde el primer elemento es la letra y su segundo elemento del par
-- es el porcentaje de cuantas veces ha aparecido en el comentario la letra del abecedario inglés
-- Para simplificar el número de tuplas, las mayúsculas se tendrán en cuenta como si fueran minúsculas
-- en su respectiva letra. Por ejemplo:

    -- lPorcentajes "Mi personaje favorito es Grogu" == [('o',13.333334),('r',10.0),('e',10.0),('s',6.666667),('i',6.666667),('g',6.666667),('a',6.666667),('v',3.3333335),('u',3.3333335),('t',3.3333335),('p',3.3333335),('n',3.3333335),('m',3.3333335),('j',3.3333335),('f',3.3333335),('z',0.0),('y',0.0),('x',0.0),('w',0.0),('q',0.0),('l',0.0),('k',0.0),('h',0.0),('d',0.0),('c',0.0),('b',0.0)]

lPorcentajes :: String -> [(Char,Float)]
lPorcentajes xs = ordenarElementos $ zip ['a'..'z'] (porcentajeCadena xs)
    where ordenarElementos tuplas = reverse $ sortBy (comparing (\(x,y) -> y)) tuplas --Dada una lista de tuplas, devolvemos otra lista ordenada de mayor a menor

-- =====================================================================
-- =====================================================================
-- (porcentajeCadena str) Es el porcentaje de uso de cada una de las letras del abecedario inglés
-- en nuestra cadena str. Por ejemplo:

-- porcentajeCadena "Programación Declarativa" == [20.833332,0.0,8.333334,4.166667,4.166667,0.0,4.166667,0.0,8.333334,0.0,0.0,4.166667,4.166667,4.166667,4.166667,4.166667,0.0,12.5,0.0,4.166667,0.0,4.166667,0.0,0.0,0.0,0.0]

porcentajeCadena :: String -> [Float]
porcentajeCadena xs = [porcentaje (ocurrencias x xs') n | x <- ['a'..'z']] 
    where xs' = limpia xs
          n = length (xs)
-- =====================================================================
-- =====================================================================
-- (ocurrencias x xs) Es el número de veces que se repite un elemento x
-- en la lista xs. Por ejemplo:
    --  ocurrencias 'W'  "QWERTY" == 1

ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']
-- =====================================================================
-- =====================================================================
-- (porcentaje s t) es el porcentaje de s sobre t. Por ejemplo:

--     porcentaje 2 10 == 20.0

porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100
-- =====================================================================
-- =====================================================================
-- Tipo de dato Abstracto Árbol, que usaremos para mostrar por pantalla los caracteres más usados
-- dentro de la review
data ArbolP = H Char | HF | N (ArbolP) (ArbolP)
  deriving Show
-- =====================================================================
-- =====================================================================
-- (arbolP str) Dado un comentario, vamos a devolver un arbol con los 5 caracteres más usados
-- dentro de la review. Por ejemplo:
    -- arbolP "QWERTZ QWERTY DVORAK AZERTY" ==  N (H 'r') (N (H 't') (N (H 'e') (N (H 'z') (H 'y'))))

arbolP :: String -> ArbolP
arbolP str
  | length ls == 1 = N (H (fst $ head ls)) HF
  | otherwise = aux ls
  where ls = take 5 $ lPorcentajes $ limpia str
-- =====================================================================
-- =====================================================================
-- (aux xs) Función auxiliar para construir el arbol de la función arbolP
aux :: [(Char,Float)] -> ArbolP
aux (x:[]) = H (fst x)
aux (x:xs) = N (H (fst x)) (aux xs)
-- =====================================================================
-- =====================================================================
-- (imprimeLetras str) Dada una review, nos va a mostrar por pantalla los 5 caracteres más usados
-- dentro del comentario en porcentaje. Por ejemplo:
-- imprimeLetras "QWERTZ QWERTY DVORAK AZERTY" == Porcentajes de las letras más usadas en el comentario 

--                                                 r : 16.666668
--                                                 t : 12.5
--                                                 e : 12.5
--                                                 z : 8.333334
--                                                 y : 8.333334

imprimeLetras :: String -> IO()
imprimeLetras str= do
    putStrLn "\n"
    putStrLn "Porcentajes de las letras más usadas en la review seleccionada: \n"
    mapM_ (\ (a,b) -> putStrLn $ concat $ [a, " : ", show b]) t
    where t = map (\(a,b) -> ([a],b)) $ take 5 $ lPorcentajes $ limpia str
-- =====================================================================
-- =====================================================================
--5)
-- (sentPolAverage rws str) Dada una lista de reviews y un tipo de sentimiento, nos devuelve la media de sentimientos
-- que han tenido todas las reviews 
sentPolAverage :: Reviews -> String -> Float
sentPolAverage (x:xs) a
        | null xs = res
        | otherwise = res + sentPolAverage xs a
        where res = if sentiment x == a then  sentiment_Polarity x else 0.0

-- (polAverage xs a) Imprime por pantalla la media de un tipo de sentimiento
polAverage :: Reviews -> String -> IO()
polAverage xs a = putStrLn $ "El \"Sentiment\" " ++ a ++ " tiene una media en su ponderación de "++ show( sentPolAverage xs a / sum [1 | x<-xs, sentiment x == a])