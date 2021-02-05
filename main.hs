-- El programa desarrollado debe ser completamente funcional y compilable con la versión de
-- Haskell 8.6. Si se usa alguna librería no empleada en la asignatura, indicar en la memoria cual y
-- cómo instalarla con cabal.
-- Para la superación del trabajo, el código debe ser programación funcional y contener
-- como mínimo, de forma natural:

-- • Dos usos de cada concepto básico de programación funcional visto en la asignatura. Es
-- decir: al menos usar 2 funciones básicas de prelude y Data.List, definir 2 funciones
-- recursivas, definir 2 funciones por patrones, 2 usos de guardas, 2 usos de case of, 2 usos
-- de listas por comprensión, 2 usos de orden superior, declaraciones de tipos para todas las
-- funciones definidas, 2 usos de evaluación perezosa, etc.
-- • Creación de un módulo
-- • Creación de dos tipos de datos nuevos y usos de éstos.
-- • Uso de al menos dos de tipos de datos abstractos o librerías vistos en la asignatura (por
-- ejemplo, pilas, colas, map, matrix, array).

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Librerias usadas
import Text.CSV -- Implementación de csv en Haskell
import Data.Default -- Librería que nos permite instanciar la clase Default
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Control.DeepSeq

-- Creamos un nuevo tipo de datos, que corresponderá cada aplicación que contiene nuestro dataset
data Aplication = App {
    app::String, category::String, rating::Maybe Float, reviews::Int, size::String, installs::Int,
    typeprice::String, price::Float, contentrating::String, genres::String, lastupdated::String,
    currentversion::String, androidver:: String

} deriving (Show, Eq)

-- Hemos creado una instancia para tener valores por defecto en el caso de que falten campos en nuestra aplicación
instance Default Aplication where
    def =  App {
    app=def, category=def, rating=def, reviews=def, size=def, installs=def,
    typeprice=def, price=def, contentrating=def, genres=def, lastupdated=def,
    currentversion=def, androidver=def
    }


-- 1º Problema: Como tratar rating, Float o String? Esto es debido a que aunque la mayoría de los elementos de la
-- zona de Rating son numeros reales, pero hay algunos que aparecen con la etiqueta NaN, como se van a tratar?

-- Solución: pasarlo como Maybe Float, si es NaN, devuelve Nothing, y si no es Just Numero.
-- Por el momento los dejaré como String.
    -- Posible solución: Usar un Maybe Int

-- 2º Problema: Price convertilo a String, o Lo ponemos como Float, le quitamos el $ y se lo ponemos cuando haga falta?
    -- Solución, ponerlo como float pero antes quitarle el $

type Aplications = [Aplication] -- lista de aplicaciones?? o creamos un nuevo tipo de datos?


main :: IO()
main =  do
    -- Guardamos el nombre de nuestro dataset
    let fileName = "googleplaystore.csv"
    -- Analizamos nuestro fichero de entrada para ver si se trata
    -- de un csv o no, dependiendo de ello lo trataremos de una manera u otra.
    csv <- parseCSVFromFile fileName
    -- La función parseCSVFromFile es de tipo Either Text.Parsec.Error.ParseError CSV
    -- Por lo que si no hay error, devuelve (Right x), siendo x de tipo csv.

    let filas = case csv of
            -- En caso de que no hubiera ningún error, se nos devolvería todas las líneas
            -- de nuestro fichero csv en una lista.
            (Right lineas) -> lineas -- [Record]
            -- En caso contrario, devolvemos []
            _ -> []

    if null filas then
        putStrLn "El fichero CSV no es válido o carece de contenido"
    else do -- significa que es un fichero valido 
        let cabecera = head filas -- Cabecera del fichero CSV -- 
        let cuerpo = tail filas -- Filas con información del CSV
        putStrLn "Atributos de cada aplicación \n"
        -- imprimeAtributos cabecera  -- Imprime la cabecera 
        -- let asd2 = consiguealista cabecera cuerpo -- Aplications
        -- ej3 asd2
        putStrLn "ASDASD"

imprimeAtributos :: Record -> IO()
imprimeAtributos xs = sequence_ $ map (\ (x,y) -> putStrLn $ concat $ [show x," : ",y]) (zip [1..] xs)

consiguealista :: [String] -> [Record] -> [String] -- ARREGLAR
consiguealista cabecera xs = [show x | x<-xs]

ej3 :: [String] -> IO()
ej3 xs = sequence_ $ map (\ (x,y) -> putStrLn $ concat $ [show x," : ",y]) (zip [1..] xs)

--Función que traduzca un record a tipo Aplication
traduccionRecord :: [String] -> Record -> Aplication -- [Photo Editor & Candy Camera & Grid & ScrapBook,ART_AND_DESIGN,4.1,159,19M,"10,000+",Free,0,Everyone,Art & Design,"January 7, 2018",1.0.0,4.0.3 and up]
traduccionRecord cabecera xs = variable
    where ts =  [(a,b) | (a,b) <- (zip cabecera xs)] -- [(app,photoeditor...), (Category,ART_AND_DESING).....]
          variable = def {
          app=(traduce ts "app"), category=(traduce ts "category"), rating=(traduceRating (traduce ts "rating")), reviews=(traduceReview (traduce ts "rating")),
          size=(traduce ts "size"), installs=(traduceInstall (traduce ts "installs")),
          typeprice=(traduce ts "typeprice"), price=(traducePrice (traduce ts "price")), contentrating=(traduce ts "contentrating"), genres=(traduce ts "genres"), lastupdated=(traduce ts "lastupdated"),
          currentversion=(traduce ts "currentversion"), androidver=(traduce ts "androidver")
          }

--"\"Photo Editor & Candy Camera & Grid & ScrapBook\"" 

traduce :: [(String,String)] -> String -> String
traduce xs str = head [show b | (a,b)<-xs, a==str]


-- Métodos Para parsear los elementos de tipo String al que nos piden en el tipo Aplication
traduceRating :: String -> Maybe Float
traduceRating str
    | str=="NaN" = Nothing
    | otherwise = Just $ read str :: Maybe Float

traduceReview :: String -> Int 
traduceReview str = read str :: Int

traduceInstall :: String -> Int
traduceInstall str = read (eliminarcoma $ init str) :: Int
    where eliminarcoma str = filter (\x -> x/=',') str

traducePrice :: String -> Float
traducePrice str 
        | (str == "0") = read str :: Float
        | otherwise = read (tail str) :: Float

-- funciones que vamos a necesitar
-- 1) Función que traduzca un record a tipo Aplication
-- 2) funcion que recorra un record
-- 3) función que según la posición de un record nos devuelva su valor correspondiente
    

-- INVESTIGACIÓN DEL MERCADO UTILIZANDO ANALISIS ESTADÍSTICOS EN UN DATASET DE APLICACIONES DE GOOGLE PLAY

--- METODOS QUE PODRIAMOS IMPLEMENTAR
-- 1) Numero de atributos que tiene nuestro dataset
-- 2) Mostrar las categorías que nos encontramos en el dataset
