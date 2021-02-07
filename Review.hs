-- Main2
-- Kenny Jesús Flores Huamán
-- Jesús Pineda Marquez
-- Universidad de Sevilla
-- Sevilla, 1 febrero de 2021
-- =====================================================================

-- INVESTIGACIÓN DEL MERCADO UTILIZANDO ANALISIS ESTADÍSTICOS EN UN DATASET DE APLICACIONES DE GOOGLE PLAY

-- - METODOS QUE PODRIAMOS IMPLEMENTAR
-- 1) Numero de atributos que tiene nuestro dataset (hecho)
-- 2) Mostrar las categorías que nos encontramos en el dataset (hecho)
-- 3) Calcular la media de las instalaciones por categoría (hecho)
-- 4) Calcular las 5 aplicaciones con mejor valoración por categoría
-- 5) Porcentaje de cuantas aplicaciones son gratuita o de pago en google play (hecho)
-- 8) Aplicaciones con más descargas (Hecho, tarda casi 1,10 min)
-- 7) Aplicación con más Installs(No es concreto debido a que hay varias con más de 1 millón) (Hecho)
-- 6) Aplicación con más rewiews (Hecho)
-- 5) Porcentaje de cuantas aplicaciones son gratuita o de pago en google play (Hecho)
-- 4) Calcular la media de del rating por categoría 
-- 3) Calcular la media de las instalaciones por categoría


module Review
    (Review,
    Reviews,
    imprimeCabecera,
    traduccionRecords,
    traduccionRecord,
    traduce,
    traducesent
    ) where

-- =====================================================================
-- MODULOS UTILIZADAS
import Text.CSV -- Implementación de csv en Haskell
import Data.Default -- Librería que nos permite instanciar la clase Default
import Data.List as L
import Data.Ord (comparing)
import Data.Maybe
-- =====================================================================


-- =====================================================================
-- TIPOS DE DATOS UTILIZADOS

-- Creación de un nuevo tipo de datos, que corresponderá con cada fila de atributos
-- que contiene nuestro dataset

data Review = Rev {
    app::String, translated_Review::String, sentiment::String, sentiment_Polarity::Float, sentiment_Subjectivity::Float

} deriving (Show, Eq)

-- Creación de una instancia para tener valores por defecto en el caso de que falten campos 
-- en nuestra aplicación
instance Default Review where
    def =  Rev {
    app=def, translated_Review=def ,sentiment=def ,sentiment_Polarity=def ,sentiment_Subjectivity=def
    }

type Reviews = [Review] -- Lista de Reviews
-- =====================================================================

-- =====================================================================
-- MAIN
main :: IO()
main =  do
    -- Guardamos el nombre de nuestro dataset
    let fileName = "ejemplos/googleplaystore_user_reviews_fixed.csv"
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
    else do -- Significa que es un fichero valido 
        let cabecera = head filas -- Cabecera del fichero CSV
        let cuerpo = tail filas -- Filas con información del CSV
        putStrLn "Atributos de cada aplicación \n"
        imprimeCabecera cabecera  -- Imprime la cabecera = ["app","translated_Review","sentiment","sentiment_Polarity","sentiment_Subjectivity"]
        let reviews = traduccionRecords cabecera cuerpo
        putStrLn $ show reviews -- Muestra por consola la lista aplicaciones
        ------------------------------------------------
        --let f1 = [(app x, show(installs x)++"+") | x<-appsMasInstalls aplications]
        --putStrLn $ show f1
        putStrLn " "
-- =====================================================================
imprimeCabecera :: Record -> IO()
imprimeCabecera xs = sequence_ $ map (\ (x,y) -> putStrLn $ concat $ [show x," : ",y]) (zip [1..] xs)



traduccionRecords :: [String] -> [Record] -> Reviews
traduccionRecords cabecera xs = [traduccionRecord cabecera x | x<-xs]
-- =====================================================================

-- =====================================================================
-- (traduccionRecord cabecera xs) dada una lista de Strings con la cabecera del csv y un Record,
-- obtiene un tipo Aplication. Por ejemplo:

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
-- (traduceRating str) es la devolución de un Maybe Float dependiendo de si se conoce el rating
-- o el dato es desconocido. Por ejemplo:
--    traduceRating "NaN" == Nothing
--    traduceRating "2.3" == Just 2.3

traducesent :: String -> Float
traducesent str = read str :: Float